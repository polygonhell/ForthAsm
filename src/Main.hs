{-# LANGUAGE RecordWildCards #-}
module Main where


import System.IO  
import Data.List.Split 
import Data.List
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Word
import Data.Bits
import Text.Printf
import Debug.Trace

data Instruction = Imm Int 
                 | Str String
                 | Branch Int | BranchNE Int | BranchTarget Int 
                 | Call String | Return
                 | Add | Sub | Mul 
                 | CmpEq
                 | Dup | Swap | Drop | Over | Drop2
                 | ToR | FromR | PeekR
                 | IError String
                 deriving (Show)

data FWord = FWord {fwName :: String, fwInst :: [Instruction], fwInline :: Bool}
           | FWordError String
           deriving (Show)

data Env = Env { base :: [FWord],
                 user :: [FWord],
                 nextLabel :: Int,
                 targetStack :: [Int]
               } deriving (Show)

data GenEnv = GenEnv { geAddr :: Int
                     , symbols :: [(String, Int)]
                     , locals :: [(Int, Int)]
                     , fwdRef :: [(Int, Int)] -- Forwards references label -> address
                     , geStrings :: [(String, [Int])] -- Strings and their Patch addresses
                     } deriving (Show)


baseWords = [ FWord "+" [Add] True
            , FWord "-" [Sub] True
            , FWord "dup" [Dup] True
            , FWord "swap" [Swap] True
            , FWord "drop" [Drop] True
            , FWord "2drop" [Drop2] True
            , FWord "over" [Over] True
            , FWord ">r" [ToR] True
            , FWord "r>" [FromR] True
            , FWord "r@" [PeekR] True
            , FWord ";" [Return] True
            , FWord "=" [CmpEq] True
            -- Compund words
            , FWord "2dup" [Over, Over] True
            ]


addInst = 0x6303 :: Word16
subInst = 0x6383 :: Word16
dupInst = 0x6031 :: Word16
swapInst = 0x60a0 :: Word16
dropInst = 0x6083 :: Word16
drop2Inst = 0x6102 :: Word16
overInst = 0x60b1 :: Word16
toRInst = 0x60e3 :: Word16
fromRInst = 0x6435 :: Word16
peekRInst = 0x6431 :: Word16
retInst = 0x6004 :: Word16
jmpInst = 0x2000 :: Word16
jmpNEInst = 0x4000 :: Word16
callInst = 0x0000 :: Word16
cmpEqInst = 0x6483 :: Word16


findLocalAddress :: GenEnv -> Int -> Maybe Word16
findLocalAddress GenEnv{..} item = addr where
    cmpfn a = fst a == item
    addr' = find cmpfn locals
    addr = addr' >>= (Just . fromIntegral . snd)

findSymbolAddress :: GenEnv -> String -> Word16
findSymbolAddress GenEnv{..} item = addr where
    cmpfn a = fst a == item
    addr' = find cmpfn symbols
    addr = fromIntegral $ snd $ fromMaybe (error (printf "couldn't find symbol %s" item)) addr'


addStringRef :: GenEnv -> String -> Int -> GenEnv
addStringRef env@GenEnv{..} str addr = env' where
    strings' = addOrUpdateStrings geStrings str addr
    env' = env{geStrings = strings'}
    addOrUpdateStrings :: [(String, [Int])] -> String -> Int -> [(String, [Int])]
    addOrUpdateStrings [] s a = [(s, [a])]
    addOrUpdateStrings ((s', as): strs) s a 
        | s' == s = (s', a:as):strs
        | otherwise = (s', as) : addOrUpdateStrings strs s a


incAddr :: GenEnv -> GenEnv
incAddr env@GenEnv{..} = env {geAddr = geAddr + 1}

addBytes:: [Word16] -> (GenEnv, [Word16]) -> (GenEnv, [Word16])
addBytes ws (env@GenEnv{..}, rws) = (env, ws ++ rws)


generateBranch :: Word16 -> Int -> GenEnv -> [Instruction] -> (GenEnv, [Word16])
generateBranch w x sym@GenEnv{..} is = addBytes [w + loc] (generateBytes (incAddr sym') is) where
    loc' = findLocalAddress sym x
    (loc, sym') = case loc' of
        Just x -> (x, sym)
        Nothing -> (0xfff, sym'') where
            sym'' = sym {fwdRef = (x, geAddr):fwdRef}


generateBytes :: GenEnv -> [Instruction] -> (GenEnv, [Word16])
generateBytes sym [] = (sym, [])

generateBytes sym (Call t : Return : is) = addBytes [jmpInst + loc] (generateBytes (incAddr sym) is) where -- Tail call becomes jump
    loc = findSymbolAddress sym t

generateBytes sym (Return : is) = addBytes [retInst] (generateBytes (incAddr sym) is)


-- Not a real instruction, just a target for a branch
generateBytes sym@GenEnv{..} (BranchTarget x : is) = generateBytes sym' is where
    sym' = sym {locals = (x, geAddr) : locals}

generateBytes sym@GenEnv{..} (Branch x : is) = generateBranch jmpInst x sym is
generateBytes sym@GenEnv{..} (BranchNE x : is) = generateBranch jmpNEInst x sym is

generateBytes sym@GenEnv{..} (Call t : is) = addBytes [callInst + loc] (generateBytes (incAddr sym) is) where
    loc = findSymbolAddress sym t

generateBytes sym@GenEnv{..} (Imm x : is) = addBytes [0x8000 .|. fromIntegral x] (generateBytes (incAddr sym) is)
-- TODO Save and Patch address
generateBytes sym@GenEnv{..} (Str s : is) = addBytes [0x8000 .|. fromIntegral (length s), 0xFFFF]  (generateBytes sym'' is) where
    sym' = addStringRef sym s (geAddr+1)
    sym'' = sym'{geAddr = geAddr+2}

generateBytes sym@GenEnv{..} (Add : is) = addBytes [addInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Sub : is) = addBytes [subInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (CmpEq : is) = addBytes [cmpEqInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Dup : is) = addBytes [dupInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Swap : is) = addBytes [swapInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Drop : is) = addBytes [dropInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Drop2 : is) = addBytes [drop2Inst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (ToR : is) = addBytes [toRInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (FromR : is) = addBytes [fromRInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (PeekR : is) = addBytes [peekRInst] (generateBytes (incAddr sym) is)
generateBytes sym@GenEnv{..} (Over : is) = addBytes [overInst] (generateBytes (incAddr sym) is)

generateBytes _ inst = error $ printf "Unknown Instruction %s" $ show inst


codeGenWord :: GenEnv -> FWord -> (GenEnv, String, [Word16])
codeGenWord env (FWord n is _) = (env', n, bytes') where 
    (env', bytes) = generateBytes env is
    -- Patch the forwards references
    bytes' = applyPatches (geAddr env) sortedPatches patchFn bytes
    patchFn w pw = (w .&. 0xe000) + pw
    patches = map refToAddr (fwdRef env')
    sortedPatches = sortBy (comparing fst) patches
    refToAddr :: (Int, Int) -> (Int, Word16)
    refToAddr (label, addr) = (addr, target) where
        target = case findLocalAddress env' label of
            Just x -> fromIntegral x
            Nothing -> error $ "Missing forwards ref " ++ show label 



lookupWord :: Env -> String -> Maybe FWord
lookupWord env@Env{..} name = found where
    userFound = find cmpfn user
    baseFound = find cmpfn base
    found = if isJust userFound then userFound else baseFound
    cmpfn (FWord n _ _) | n == name = True
    cmpfn _ = False

shouldInline :: FWord -> Bool
shouldInline FWord{..} = fwInline



getString :: [String] -> (String, [String])
getString ts = (unwords strs, resids) where
    getString' :: [String] -> ([String], [String])
    getString' [] = ([],[])
    getString' (t:ts) 
        | last t == '\"' = ([take (length t - 1) t], ts)
        | otherwise = (t : rStrs, resids) 
        where
            (rStrs, resids) = getString' ts
    (strs, resids) = getString' ts

processToken :: Env -> [String] -> (Env, [Instruction], [String])
processToken env@Env{..} (x:xs) 
    | wordExists && shouldInline (fromJust inst) = (env, instructions, xs)
    | wordExists = (env, [Call x], xs)
    | isLiteral = (env, [Imm literal], xs)
    | otherwise = 
        case lowerX of 
            "s\"" -> (env, [Str stringLit], remTokens)
            "if" ->  (env', [BranchNE nextLabel], xs) where
                env' = env { nextLabel = nextLabel + 1, targetStack = nextLabel : targetStack }
            "else" -> (env', [Branch nextLabel, BranchTarget (head targetStack)], xs) where
                env' = env { nextLabel = nextLabel + 1, targetStack = nextLabel : tail targetStack }
            "then" -> (env', [BranchTarget (head targetStack)], xs) where
                env' = env { targetStack = tail targetStack }
            "do" -> (env', [Swap, BranchTarget nextLabel, ToR, ToR], xs) where
                env' =  env { nextLabel = nextLabel + 1, targetStack = nextLabel : tail targetStack }
            "loop" -> (env', [FromR, Imm 1, Add, FromR, Over, Over, Sub, BranchNE (head targetStack), Drop2], xs) where
                env' =  env { targetStack = tail targetStack }
            _  -> (env, [IError $ "Undefined Word " ++ x], [])
    where
        inst = lookupWord env $ map toLower x
        wordExists = isJust inst
        Just (FWord _ instructions _) = inst
        litParse = reads x :: [(Int, String)]
        isLiteral = length litParse == 1 && snd (head litParse) == ""
        [(literal, _)] = litParse
        lowerX = map toLower x
        (stringLit, remTokens) = getString xs


compileTokens :: Env -> [String] -> [Instruction]
compileTokens _ [] = []
compileTokens env t = insts ++ compileTokens env' ts where
    (env', insts, ts) = processToken env t

compileDefinition :: [String] -> Env -> FWord
compileDefinition (t:ts) env  = newWord where
    instructions = compileTokens env ts :: [Instruction]
    newWord = FWord t instructions False

getDefinitions :: [String] -> [[String]]
getDefinitions  = splitOn [":"] 


compile :: GenEnv -> [FWord] -> (GenEnv, [Word16])
compile env [] = (env, [])
compile env@GenEnv{..} (w:ws) = (env''', wOut ++ wsOut) where
    (env', _, wOut) = codeGenWord env w
    env'' = env' {symbols = (fwName w, geAddr) : symbols, locals = [], fwdRef = []} 
    (env''', wsOut) = compile env'' ws


compileString :: String -> [Word16]
compileString [] = []
compileString (a:b:cs) = fromIntegral (fromEnum a) `shiftL` 8 + fromIntegral (fromEnum b) : compileString cs
compileString [a] = [fromIntegral (fromEnum a) `shiftL` 8]

compileStringEntry :: Int -> (String, [Int]) -> (Int, [(Int, Word16)], [Word16])
compileStringEntry addr (str, patchAddrs) = (addr', patches, bytes) where
    bytes = compileString str
    addr' = addr + length bytes
    patches = map fn patchAddrs
    fn i = (i, fromIntegral addr .|. 0x8000) 

compileStrings :: Int -> [(String, [Int])] -> ([(Int, Word16)],[Word16])
compileStrings _ [] = ([], [])
compileStrings addr (str:strs) = (p ++ p', b ++ padding ++ b') where 
    (addr', p, b) = compileStringEntry addr str
    addr'' = roundTo2 addr'
    padding = replicate (addr'' - addr') 0
    (p', b') = compileStrings addr'' strs -- Strings must be 4 byte aligned

applyPatches :: Int -> [(Int, Word16)] -> (Word16 -> Word16 -> Word16) -> [Word16] -> [Word16]
applyPatches addr _ _ [] = []
applyPatches addr [] _ ws = ws
applyPatches addr patches@((pAddr, pw):ps) fn (w:ws)
    | pAddr < addr = error "Patches not sorted or duplicate patch"
    | pAddr == addr = fn w pw : applyPatches (addr+1) ps fn ws
    | otherwise = w : applyPatches (addr+1) patches fn ws



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

roundTo2 :: Int -> Int
roundTo2 x = ((x+1) `quot` 2) * 2

main ::  IO ()
main = do
    handle <- openFile "test.f" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        defs = tail $ getDefinitions singlewords
        combine :: Env -> [String] -> Env 
        combine env@Env{..} d = Env base (compileDefinition d env : user)  0 [] 
        compiledDefsR = foldl combine (Env baseWords [] 0 []) defs
        compiledDefs = reverse $ user compiledDefsR

    print compiledDefs
    
    let (env, uCompiled) = compile (GenEnv 2 [] [] [] []) compiledDefs
        (_, boot) = generateBytes (env {geAddr = 0}) bootCode
        compiled = boot ++ uCompiled

    -- Stick the Strings on the End
    let stringBase = roundTo2 $ length compiled
        (patches, compiledStrings) = compileStrings stringBase (geStrings env)
        sortedPatches = sortBy (comparing fst) patches
        patched = applyPatches 0 sortedPatches patchedValue compiled
        patchedValue _ y = y

    print $ "Patches = " ++ show sortedPatches
    print $ "StringBaseAddr = " ++ show stringBase





    let memoryImage = patched ++ replicate (stringBase - length patched) 0  ++ compiledStrings
    print $ symbols env
    print $ geStrings env
    print $ word16toHex $ compiled ++ compiledStrings
    print $ word16toHex memoryImage
    putStr $ word16toBinLines memoryImage




    hClose handle 
    putStr "Hello"