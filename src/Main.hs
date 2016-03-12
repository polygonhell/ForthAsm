{-# LANGUAGE RecordWildCards #-}
module Main where


import System.IO  
import Data.List.Split 
import Data.List
import Data.Char
import Data.Maybe
import Data.Word
import Data.Bits
import Text.Printf

data Instruction = Imm Int 
                 | Branch Int | BranchTarget Int 
                 | Call String | Return
                 | Add | Sub | Mul 
                 | Dup | Swap | Drop
                 | IError String
                 deriving (Show)

data FWord = FWord {fwName :: String, fwInst :: [Instruction], fwInline :: Bool}
           | FWordError String
           deriving (Show)

data Env = Env { base :: [FWord],
                 user :: [FWord]
               } deriving (Show)

data GenEnv = GenEnv { geAddr :: Int
                     , symbols :: [(String, Int)]
                     , locals :: [(Int, Int)]
                     }


baseWords = [ FWord "+" [Add] True
            , FWord "dup" [Dup] True
            , FWord "swap" [Swap] True
            , FWord "drop" [Drop] True
            , FWord ";" [Return] True
            ]


addInst = 0x6303 :: Word16
dupInst = 0x6031 :: Word16
swapInst = 0x60a0 :: Word16
dropInst = 0x6083 :: Word16
retInst = 0x6004 :: Word16
jmpInst = 0x2000 :: Word16
callInst = 0x0000 :: Word16


findLocalAddress :: GenEnv -> Int -> Word16
findLocalAddress GenEnv{..} item = addr where
    cmpfn a = fst a == item
    addr' = find cmpfn locals
    addr = fromIntegral $ snd $ fromMaybe (0, 0) addr'

findSymbolAddress :: GenEnv -> String -> Word16
findSymbolAddress GenEnv{..} item = addr where
    cmpfn a = fst a == item
    addr' = find cmpfn symbols
    addr = fromIntegral $ snd $ fromMaybe (error (printf "couldn't find symbol %s" item)) addr'


incAddr :: GenEnv -> GenEnv
incAddr env@GenEnv{..} = env {geAddr = geAddr + 1}

generateBytes :: GenEnv -> [Instruction] -> [Word16]
generateBytes sym [] = []

generateBytes sym (Call t : Return : is) = jmpInst + loc : generateBytes (incAddr sym) is where -- Tail call becomes jump
    loc = findSymbolAddress sym t
generateBytes sym (Return : is) = retInst : generateBytes (incAddr sym) is

-- Not a real instruction, just a target for a branch
generateBytes sym@GenEnv{..} (BranchTarget x : is) = generateBytes sym' is where
    sym' = sym {locals = (x, geAddr) : locals}
generateBytes sym@GenEnv{..} (Branch x : is) = jmpInst + loc : generateBytes (incAddr sym) is where
    loc = findLocalAddress sym x

generateBytes sym@GenEnv{..} (Call t : is) = callInst + loc : generateBytes (incAddr sym) is where
    loc = findSymbolAddress sym t

generateBytes sym@GenEnv{..} (Imm x : is) = (0x8000 .|. fromIntegral x) : generateBytes (incAddr sym) is

generateBytes sym@GenEnv{..} (Add : is) = addInst : generateBytes (incAddr sym) is
generateBytes sym@GenEnv{..} (Dup : is) = dupInst : generateBytes (incAddr sym) is
generateBytes sym@GenEnv{..} (Swap : is) = swapInst : generateBytes (incAddr sym) is
generateBytes sym@GenEnv{..} (Drop : is) = dropInst : generateBytes (incAddr sym) is

generateBytes _ inst = error $ printf "Unknown Instruction %s" $ show inst


codeGenWord :: GenEnv -> FWord -> (String, [Word16])
codeGenWord env (FWord n is _) = (n, generateBytes env is)


lookupWord :: Env -> String -> Maybe FWord
lookupWord env@Env{..} name = found where
    userFound = find cmpfn user
    baseFound = find cmpfn base
    found = if isJust userFound then userFound else baseFound
    cmpfn (FWord n _ _) | n == name = True
    cmpfn _ = False

shouldInline :: FWord -> Bool
shouldInline FWord{..} = fwInline

processToken :: Env -> String -> [Instruction]
processToken env x 
    | wordExists && shouldInline (fromJust inst) = instructions
    | wordExists = [Call x]
    | isLiteral = [Imm literal]
    | otherwise = [IError $ "Undefined Word " ++ x]
    where
        inst = lookupWord env x
        wordExists = isJust inst
        Just (FWord _ instructions _) = inst
        litParse = reads x :: [(Int, String)]
        isLiteral = length litParse == 1 && snd (head litParse) == ""
        [(literal, _)] = litParse

compileDefinition :: [String] -> Env -> FWord
compileDefinition (t:ts) env  = newWord where
    instructions = foldMap (processToken env) ts :: [Instruction]
    newWord = FWord t instructions False

getDefinitions :: [String] -> [[String]]
getDefinitions  = splitOn [":"] 


compile :: GenEnv -> [FWord] -> (GenEnv, [Word16])
compile env [] = (env, [])
compile env@GenEnv{..} (w:ws) = (env'', wOut ++ wsOut) where
    wOut = snd $ codeGenWord env w
    env' = env { geAddr = geAddr + length wOut, symbols = (fwName w, geAddr) : symbols }
    (env'', wsOut) = compile env' ws

word16toHex :: [Word16] -> String
word16toHex [] = ""
word16toHex (w : ws) = hex ++ word16toHex ws where
    hex = printf "%04x " w

word16toBinLines :: [Word16] -> String
word16toBinLines ws = unlines $ map fn (pairWords ws) where
    fn = printf "%032b"
    pairWords (w1 : w2 : ws) = toInteger w1 `shiftL` 16 + toInteger w2 : pairWords ws
    pairWords [w1] = [toInteger w1 `shiftL` 16]
    pairWords [] = []

bootCode :: [Instruction]
bootCode = [Call "main", BranchTarget 0, Branch 0]

main ::  IO ()
main = do
    handle <- openFile "test.f" ReadMode
    contents <- hGetContents handle
    let normContents = map toLower contents
        singlewords = words normContents
        defs = tail $ getDefinitions singlewords
        combine :: Env -> [String] -> Env 
        combine env@Env{..} d = Env base (compileDefinition d env : user)
        compiledDefsR = foldl combine (Env baseWords []) defs
        compiledDefs = reverse $ user compiledDefsR

    print compiledDefs
    
    let (env, uCompiled) = compile (GenEnv 2 [] []) compiledDefs
        boot = generateBytes (env {geAddr = 0}) bootCode
        compiled = boot ++ uCompiled



    print $ symbols env
    print $ word16toHex compiled
    putStr $ word16toBinLines compiled




    hClose handle 
    putStr "Hello"