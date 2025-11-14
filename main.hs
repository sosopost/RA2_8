{-
    PUCPR - Programação Lógica e Funcional
    Atividade RA2 - Funcional em Haskell
    Nome: Sophia Post Ploposki.
-}
-- Importações 
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.IO
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, SomeException, toException, IOException, evaluate)
import Text.Read (readMaybe)
import Data.List (sortBy, groupBy, isInfixOf, maximumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.IO.Error (isDoesNotExistError, catchIOError)
import Data.Char (isSpace)
import System.Directory (doesFileExist)

----------------------------------------------------------------------
-- PARTE 1: ARQUITETO DE DADOS
----------------------------------------------------------------------
data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read, Eq, Ord)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq, Ord)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq, Ord)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read, Eq, Ord)

type ResultadoOperacao = (Inventario, LogEntry)

----------------------------------------------------------------------
-- PARTE 2: LÓGICA DE NEGÓCIO PURA 
----------------------------------------------------------------------
arqInventario :: FilePath
arqInventario = "Inventario.dat"

arqAuditoria :: FilePath
arqAuditoria = "Auditoria.log"

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem time id nome qtd cat inv =
    if Map.member id inv
    then Left $ "Falha: Item com ID '" ++ id ++ "' ja existe."
    else
        let novoItem = Item id nome qtd cat
            novoInv = Map.insert id novoItem inv
            logEntry = LogEntry time Add ("ItemID: " ++ id ++ ", Adicionado: " ++ nome) Sucesso
        in Right (novoInv, logEntry)


updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time id novaQtd inv =
    case Map.lookup id inv of
        Nothing -> Left $ "Falha: Item com ID '" ++ id ++ "' nao encontrado."
        Just item ->
            if novaQtd < 0
            then Left $ "Valores negativos não são permitidos."
            else
                let
                    (novoInv, logMsg) =
                        if novaQtd == 0
                        then ( Map.delete id inv
                             , "ItemID: " ++ id ++ ", Removido (Estoque Zerado): " ++ nome item
                             )
                        else ( Map.adjust (\i -> i { quantidade = novaQtd }) id inv
                             , "ItemID: " ++ id ++ ", Quantidade alterada para " ++ show novaQtd ++ " de '" ++ nome item ++ "'"
                             )

                    logEntry = LogEntry time Update logMsg Sucesso
                
                in Right (novoInv, logEntry)
                
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time id qtdRemover inv =
    case Map.lookup id inv of
        Nothing -> Left $ "Falha: Item com ID '" ++ id ++ "' nao encontrado."
        Just item ->
            if quantidade item < qtdRemover
            then Left $ "Falha: Estoque insuficiente para '" ++ nome item ++ "'. (Disponivel: " ++ show (quantidade item) ++ ", Solicitado: " ++ show qtdRemover ++ ")"
            else
                let novaQtd = quantidade item - qtdRemover
                    (novoInv, logMsg) = if novaQtd == 0
                                        then (Map.delete id inv, "ItemID: " ++ id ++ ", Removido (Estoque Zerado): " ++ nome item)
                                        else (Map.adjust (\i -> i { quantidade = novaQtd }) id inv,
                                              "ItemID: " ++ id ++ ", Removido " ++ show qtdRemover ++ " de '" ++ nome item ++ "'")
                    logEntry = LogEntry time Remove logMsg Sucesso
                in Right (novoInv, logEntry)

listarItens :: Inventario -> String
listarItens inv
    | Map.null inv = "Inventario esta vazio."
    | otherwise = unlines $ map formatarItem (Map.elems inv)
    where
        formatarItem i = "ID: " ++ itemID i ++ ", Nome: " ++ nome i ++ ", Qtd: " ++ show (quantidade i) ++ ", Cat: " ++ categoria i

----------------------------------------------------------------------
-- PARTE 4: VALIDAÇÃO E RELATÓRIOS 
----------------------------------------------------------------------
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (ehFalha . status)
    where ehFalha (Falha _) = True
          ehFalha Sucesso = False

formatarLogsDeErro :: [LogEntry] -> String
formatarLogsDeErro logs
    | null logs = "Nenhum erro registrado."
    | otherwise = unlines $ map formatarLog logs
    where
        formatarLog log = show (timestamp log) ++ " - " ++ extrairErro (status log)
        extrairErro (Falha msg) = msg
        extrairErro Sucesso = ""

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idAlvo logs =
    filter (\log ->
        acao log `elem` [Add, Remove, Update] &&
        ("ItemID: " ++ idAlvo) `isInfixOf` detalhes log
    ) logs

extrairIdDoDetalhe :: String -> Maybe String
extrairIdDoDetalhe str =
    let partes = words $ replace ',' ' ' str  -- Trata vírgulas como espaços
    in case partes of
        ("ItemID:" : id : _) -> Just id
        _                    -> Nothing
  where
    replace a b = map (\c -> if c == a then b else c)

itemMaisMovimentado :: Inventario -> [LogEntry] -> String
itemMaisMovimentado _ [] = "Nenhuma movimentação registrada."
itemMaisMovimentado inv logs =
    let movimentacoes = filter (\l -> acao l `elem` [Add, Remove]) logs
        pares = mapMaybe (\l -> (,) <$> extrairIdDoDetalhe (detalhes l) <*> pure l) movimentacoes
        grupos = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) pares
        contagens = [ (fst x, length g) | g@(x:_) <- grupos ]
    in case contagens of
         [] -> "Nenhuma movimentação registrada."
         _  -> let (idTop, qtd) = maximumBy (comparing snd) contagens
                   nomeItem = maybe ("ID: " ++ idTop) nome $ Map.lookup idTop inv
               in nomeItem ++ " (ID: " ++ idTop ++ ") -> " ++ show qtd ++ " movimentações"

----------------------------------------------------------------------
-- PARTE 3: MÓDULO DE I/O E PERSISTÊNCIA
----------------------------------------------------------------------
carregarInventario :: IO Inventario
carregarInventario = (do
    conteudo <- readFile arqInventario
    case readMaybe conteudo of
        Nothing -> putStrLn "Aviso: Inventario.dat corrompido" >> return Map.empty
        Just inv -> return inv
    ) `catchIOError` lidarExcecaoLeitura Map.empty

carregarLogs :: IO [LogEntry]
carregarLogs = catch readLog handler
  where
    readLog = do
      exists <- doesFileExist arqAuditoria
      if not exists then return []
      else do
        s <- withFile arqAuditoria ReadMode $ \h -> do
          content <- hGetContents h
          evaluate (length content `seq` content)
        let ls = filter (not . all isSpace) (lines s)
        return $ mapMaybe readMaybe ls
    handler :: IOException -> IO [LogEntry]
    handler _ = return []

lidarExcecaoLeitura :: a -> IOError -> IO a
lidarExcecaoLeitura valorPadrao err
    | isDoesNotExistError err = do
        putStrLn "Aviso: Arquivos não encontrados. Iniciando com estado vazio."
        return valorPadrao
    | otherwise = do
        putStrLn $ "Erro ao ler arquivo: " ++ show err
        return valorPadrao

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile arqInventario (show inv)

salvarLog :: [LogEntry] -> LogEntry -> IO [LogEntry]
salvarLog logsAntigos novoLog = do
    let novosLogs = logsAntigos ++ [novoLog]
    appendFile arqAuditoria (show novoLog ++ "\n")
    return novosLogs

main :: IO ()
main = do
    putStrLn "Iniciando Sistema de Inventario..."
    invInicial <- carregarInventario
    logsIniciais <- carregarLogs
    putStrLn "Sistema carregado. Digite 'ajuda' para ver os comandos."
    loopPrincipal (invInicial, logsIniciais)

loopPrincipal :: (Inventario, [LogEntry]) -> IO ()
loopPrincipal (inventario, logs) = do
    putStr "> "
    hFlush stdout
    linha <- getLine
    if null linha
    then loopPrincipal (inventario, logs)
    else do
        let comando = words linha
        timeAtual <- getCurrentTime
        processarComando timeAtual comando (inventario, logs)

processarComando :: UTCTime -> [String] -> (Inventario, [LogEntry]) -> IO ()
processarComando time comando (inv, logs) =
    case comando of
        ["add", id, nome, qtdStr, cat] -> do
            case readMaybe qtdStr of
                Nothing -> lidarFalha "Quantidade invalida."
                Just qtd -> case addItem time id nome qtd cat inv of
                    Left erro -> lidarFalha erro
                    Right (novoInv, logEntry) -> lidarSucesso novoInv logEntry

        ["update", id, qtdStr] -> do
            case readMaybe qtdStr of
                Nothing -> lidarFalha "Quantidade invalida."
                Just qtd -> case updateQty time id qtd inv of
                    Left erro -> lidarFalha erro
                    Right (novoInv, logEntry) -> lidarSucesso novoInv logEntry

        ["remove", id, qtdStr] -> do
            case readMaybe qtdStr of
                Nothing -> lidarFalha "Quantidade invalida."
                Just qtd -> case removeItem time id qtd inv of
                    Left erro -> lidarFalha erro
                    Right (novoInv, logEntry) -> lidarSucesso novoInv logEntry

        ["listar"] -> do
            putStrLn "--- Inventario Atual ---"
            putStrLn $ listarItens inv
            putStrLn "------------------------"
            loopPrincipal (inv, logs)

        ["report"] -> do
            putStrLn "--- Relatorio de Erros ---"
            putStrLn $ formatarLogsDeErro $ logsDeErro logs
            putStrLn "--- Item Mais Movimentado ---"
            putStrLn $ itemMaisMovimentado inv logs
            putStrLn "-----------------------------"
            loopPrincipal (inv, logs)

        ["historico", id] -> do
            let hist = historicoPorItem id logs
            putStrLn $ "--- Historico do Item ID: " ++ id ++ " ---"
            if null hist
            then putStrLn "Nenhuma movimentacao registrada para este item."
            else putStrLn $ unlines $ map (\l -> show (timestamp l) ++ " | " ++ show (acao l) ++ " | " ++ detalhes l) hist
            putStrLn "-------------------------------------"
            loopPrincipal (inv, logs)

        ["ajuda"] -> do
            exibirAjuda
            loopPrincipal (inv, logs)

        ["sair"] -> putStrLn "Encerrando sistema."

        _ -> do
            putStrLn $ "Comando invalido: '" ++ unwords comando ++ "'. Digite 'ajuda'."
            loopPrincipal (inv, logs)

    where
        lidarFalha erro = do
            putStrLn $ "Erro: " ++ erro
            let logFalha = LogEntry time QueryFail ("Comando: " ++ unwords comando) (Falha erro)
            novosLogs <- salvarLog logs logFalha
            loopPrincipal (inv, novosLogs)

        lidarSucesso novoInv logSucesso = do
            putStrLn $ "Sucesso: " ++ detalhes logSucesso
            salvarInventario novoInv
            novosLogs <- salvarLog logs logSucesso
            loopPrincipal (novoInv, novosLogs)

exibirAjuda :: IO ()
exibirAjuda = do
    putStrLn "--- Comandos Disponiveis ---"
    putStrLn "add <id> <nome> <qtd> <categoria> - Adiciona um novo item"
    putStrLn "update <id> <qtd> - Atualiza quantidade do item (remove se quantidade = 0)"
    putStrLn "remove <id> <qtd> - Remove quantidade do item (remove se estoque zerar)"
    putStrLn "listar - Lista todos os itens"
    putStrLn "report - Exibe relatorios (erros + item mais movimentado)"
    putStrLn "historico <id> - Mostra historico de um item"
    putStrLn "ajuda - Mostra esta mensagem"
    putStrLn "sair - Encerra e salva"
    putStrLn "----------------------------"