## 1. Introdução

Este relatório descreve a implementação do jogo de damas utilizando Haskell. O projeto visa demonstrar como os paradigmas funcionais podem ser aplicados efetivamente no desenvolvimento de jogos estratégicos, explorando conceitos como imutabilidade, tipos de dados e recursão.

## 2. Arquitetura e Modelagem de Dados

### 2.1 Tipos de Dados Fundamentais

A modelagem do jogo foi feita a partir de tipos de dados que representam de forma clara todos os elementos do domínio:

```haskell
-- Tipos básicos do jogo
data Piece = Pawn | King deriving (Eq, Show)
data Player = Red | Black deriving (Eq, Show)
data Square = Empty | Occupied Player Piece deriving (Eq, Show)

-- Tipos compostos
type Position = (Int, Int)
type Board = [[Square]]
type Move = (Position, Position)
```

**Justificativa das Escolhas:**

- **`Piece`**: Enum simples representando os dois tipos de peças (peão e rei)
- **`Player`**: Identificação dos jogadores, facilitando alternância de turnos
- **`Square`**: Tipo que pode estar vazio ou ocupado, eliminando estados inválidos
- **`Position`**: Tupla (linha, coluna) fornece acesso direto às coordenadas
- **`Board`**: Lista de listas oferece acesso direto pelas coordenadas
- **`Move`**: Par de posições (origem, destino) que representam o movimento

### 2.2 Estado do Jogo

```haskell
data GameState = GameState
    { board :: Board
    , currentPlayer :: Player
    , gameOver :: Bool
    , winner :: Maybe Player
    } deriving (Show)
```

O `GameState` centraliza todo o estado do jogo através de um record, permitindo:
- Acesso nomeado aos campos
- Atualizações parciais com sintaxe `record { field = newValue }`
- Agrupamento do estado completo em uma única estrutura

### 2.3 Configuração e Modos de Jogo

```haskell
data GameMode = HumanVsAI | AIVsAI deriving (Eq, Show)
```

Enum para diferentes modos de jogo, permitindo implementações futuras (ex: HumanVsHuman, NetworkPlay).

## 3. Algoritmos e Estratégias Implementadas

### 3.1 Geração de Movimentos

A geração de movimentos segue uma arquitetura hierárquica:

```haskell
getAllMoves :: Board -> Player -> [Move]
getAllMoves board player =
    let pieces = getPlayerPieces board player
        simpleMoves = [move | pos <- pieces, move <- getSimpleMoves board player pos]
        captureMoves = [move | pos <- pieces, (move, _) <- getCaptureMovesFrom board player pos]
    in if null captureMoves then simpleMoves else captureMoves
```

**Estratégia Adotada:**
1. **Prioridade de Capturas**: Movimentos de captura têm precedência absoluta sobre movimentos simples
2. **Geração de dados**: Aproveitam o lazy evaluation do Haskell
3. **Separação de Responsabilidades**: Funções especializadas para cada tipo de movimento (Single Responsibility Principle)

### 3.2 Validação de Movimentos

```haskell
isValidMove :: Board -> Player -> Move -> Bool
isValidMove board player move = move `elem` getAllMoves board player
```

**Abordagem Funcional:**
- Validação por membership em lista de movimentos válidos
- Reutiliza a lógica de geração, garantindo consistência
- Aproveitamento do polimorfismo de `elem`

### 3.3 Algoritmo Minimax para IA

```haskell
minimax :: Board -> Player -> Int -> Bool -> Int
minimax board player depth maximizing
    | depth == 0 = evaluateBoard board player
    | otherwise =
        let moves = getAllMoves board player
        in if null moves
           then if maximizing then -1000 else 1000
           else 
               let evaluateMove move = 
                       let newBoard = executeMove board player move
                       in minimax newBoard (opponent player) (depth - 1) (not maximizing)
               in if maximizing
                  then maximum (map evaluateMove moves)
                  else minimum (map evaluateMove moves)
```

**Características da Implementação:**
- **Recursão com Memorização Implícita**: A natureza funcional evita efeitos colaterais
- **Poda Natural**: Depth-limited search com profundidade configurável
- **Avaliação Heurística**: Função `evaluateBoard` considera material e posicionamento
- **Estratégia Min-Max Clássica**: Alternância entre maximização e minimização, muito utilizada em diversos jogos.

### 3.4 Função de Avaliação

```haskell
evaluateBoard :: Board -> Player -> Int
evaluateBoard board player =
    let redPieces = getPlayerPieces board Red
        blackPieces = getPlayerPieces board Black
        redScore = sum [pieceValue (board !! r !! c) | (r, c) <- redPieces]
        blackScore = sum [pieceValue (board !! r !! c) | (r, c) <- blackPieces]
    in case player of
        Red -> redScore - blackScore
        Black -> blackScore - redScore
  where
    pieceValue (Occupied _ Pawn) = 1
    pieceValue (Occupied _ King) = 3
    pieceValue Empty = 0
```

**Heurísticas Implementadas:**
- **Contagem Material**: Peões valem 1 ponto, damas valem 3 pontos
- **Perspectiva Relativa**: Pontuação sempre da perspectiva do jogador atual

## 4. Controle de Estado do Jogo

### 4.1 Imutabilidade e Atualizações de Estado

Em Haskell, o estado é gerenciado através de imutabilidade:

```haskell
-- Aplicação de movimento retorna novo tabuleiro
applyMove :: Board -> Move -> Board
applyMove board ((fromR, fromC), (toR, toC)) =
    case getSquare board (fromR, fromC) of
        Just piece ->
            let newBoard = setSquare board (fromR, fromC) Empty
                finalPiece = promotePiece piece (toR, toC)
            in setSquare newBoard (toR, toC) finalPiece
        Nothing -> board
```

**Vantagens da Abordagem:**
- **Thread Safety**: Estruturas imutáveis são thread-safe
- **Debugging**: Estados anteriores preservados, facilitando o debug
- **Rollback**: Possibilidade de desfazer movimentos sem complexidade adicional

### 4.2 Detecção de Fim de Jogo

```haskell
checkGameOver :: GameState -> GameState
checkGameOver gameState
    | gameOver gameState = gameState  -- Já terminou
    | otherwise =
        let redPieces = getPlayerPieces (board gameState) Red
            blackPieces = getPlayerPieces (board gameState) Black
            currentMoves = getAllMoves (board gameState) (currentPlayer gameState)
        in if null redPieces
           then gameState { gameOver = True, winner = Just Black }
           else if null blackPieces
           then gameState { gameOver = True, winner = Just Red }
           else if null currentMoves
           then gameState { gameOver = True, winner = Just (opponent (currentPlayer gameState)) }
           else gameState
```

**Condições de Vitória Implementadas:**
1. **Eliminação Total**: Todos os peões/damas de um jogador foram capturados
2. **Bloqueio Completo**: Jogador atual sem movimentos válidos disponíveis
3. **Estado Já Finalizado**: Evita processamento desnecessário

### 4.3 Loop Principal do Jogo

```haskell
playGame :: GameMode -> GameState -> IO ()
playGame mode gameState = do
    putStrLn $ showBoard (board gameState)
    putStrLn $ "Jogador atual: " ++ show (currentPlayer gameState)
    
    let checkedGameState = checkGameOver gameState
    
    if gameOver checkedGameState
    then case winner checkedGameState of
        Just w -> putStrLn $ "Jogo terminado! Vencedor: " ++ show w
        Nothing -> putStrLn "Jogo terminado! Empate!"
    else do
        when (mode == AIVsAI) $ do
            putStrLn "Pressione Enter para continuar..."
            _ <- getLine
            return ()
            
        newGameState <- if mode == HumanVsAI && currentPlayer checkedGameState == Red
                       then humanMove checkedGameState
                       else aiMove gameState
        playGame mode newGameState
```

**Arquitetura do Loop:**
- **Evita efeitos colaterais**: Separação entre lógica e efeitos colaterais
- **Recursão**: Loop implementado via recursão de cauda
- **Pattern Matching**: Decisão baseada no modo de jogo
- **Estado Funcional**: Cada iteração recebe e retorna um novo estado

## 5. Tratamento de Entrada e Interface

### 5.1 Parsing de Movimentos

```haskell
parseMove :: String -> Maybe Move
parseMove input =
    case words input of
        [from, to] -> do
            fromPos <- parsePosition from
            toPos <- parsePosition to
            return (fromPos, toPos)
        _ -> Nothing

parsePosition :: String -> Maybe Position
parsePosition [r, c] = do
    row <- charToInt r
    col <- charToInt c
    if isValidPosition (row, col) then Just (row, col) else Nothing
parsePosition _ = Nothing
```

**Estratégia de Parsing:**
- **Maybe**: Tratamento de falhas de parsing
- **Validação Integrada**: Verificação de limites durante o parsing

### 5.2 Interface de Usuário

A interface utiliza IO simples:
- **Exibição usando ASCII**: Representação clara do tabuleiro
- **Input Validado**: Retry para comandos inválidos
- **Feedback Imediato**: Mensagens de erro claras

## 6. Conclusões e Considerações Técnicas

### 6.1 Vantagens da Abordagem Funcional
1. **Correto por Construção**: O sistema de tipos elimina possíveis bugs
2. **Reutilização**: Funções pequenas que facilitam teste e reutilização

### 6.2 Desafios Encontrados
1. **Performance**: Estruturas imutáveis gastam mais recursos comparado a arrays mutáveis
2. **Curva de Aprendizado**: É bem diferente de como se escreveria esse jogo em uma linguagem imperativa, além da própria notação e estruturas do haskell
3. **Debugging**: É mais complicado de entender o que está acontecendo ao debugar

### 6.3 Possíveis Melhorias
1. **Otimização**: Uso de estruturas de dados mais eficientes (ex: Vector ao invés de listas)
2. **IA Avançada**: Implementação de algoritmos como Alpha-Beta Pruning
3. **Interface Gráfica**: Migração para interface gráfica usando bibliotecas como Gtk2hs
4. **Multiplayer**: Extensão para jogos em rede
5. **Configurabilidade**: Tamanhos de tabuleiro variáveis

### 6.4 Avaliação Geral

O projeto serve como excelente exemplo de como princípios funcionais podem ser aplicados a problemas do mundo real, resultando em código robusto e elegante que demonstra o poder expressivo da programação funcional.