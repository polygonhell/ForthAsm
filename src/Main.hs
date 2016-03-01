{-# LANGUAGE RecordWildCards #-}
module Main where


import System.IO  
import Data.List.Split 
import Data.List
import Data.Char
import Data.Maybe
import Data.Word

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

type GenEnv = [(String, Int)]


baseWords = [ FWord "+" [Add] True
            , FWord "dup" [Dup] True
            , FWord "swap" [Swap] True
            , FWord "drop" [Drop] True
            , FWord ";" [Return] True
            ]


generateBytes :: GenEnv -> [Instruction] -> [Word16]
-- Tail Call
generateBytes sym (Call t : Return: []) = [0x4000 + 54] -- Tail call becomes jump

generateBytes sym (Imm x : is) = fromIntegral x : generateBytes sym is

generateBytes sym (Add : is) = 0x1234 : generateBytes sym is

generateBytes sym (Dup : is) = 0x2345 : generateBytes sym is

generateBytes sym (Call t : is) = 0x6000 + 54 : generateBytes sym is
generateBytes sym (Return:[]) = [0x0600] 
generateBytes sym inst = []

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
getDefinitions tokens = splitOn [":"] tokens

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
    
    let compiled = map (codeGenWord []) compiledDefs

    print compiled



    hClose handle 
    putStr "Hello"