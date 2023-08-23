module Tfinal where
     import System.IO
     import Control.Monad

     
     main :: IO()
     main = do                                                                                           --Menu de escolha
           putStrLn "Escolha uma opçao:"
           putStrLn "1 - Tarefa 1-Todos Alunos nas Uc"
           putStrLn "2 - Tarefa 2-Todas as Ucs de cada Aluno"
           putStrLn "3 - Tarefa 3-Filtar por Uc"
           putStrLn "4 - Tarefa 4-Filtrar por Aluno"
           opcao <- getLine
           chamaopcao opcao

     chamaopcao :: String -> IO()
     chamaopcao opcao                                                                                    --Opção correspondente á escolha
                | opcao == "1" = tarefa1
                | opcao == "2" = tarefa2
                | opcao == "3" = le_ucs2
                | opcao == "4" = le_inscricoes
                | otherwise = error "Opcao invalida"

     le_ucs2 :: IO()
     le_ucs2 = do                                                                                        --Escolher a UC pelo qual queremos filtrar
                conteudo <- readFile "ucs.txt"
                putStrLn "Insira um numero de uma UC (1 a 4)"
                putStrLn "1 - Programação Funcional"
                putStrLn "2 - Compiladores"
                putStrLn "3 - Topicos"
                putStrLn "4 - Fisica"
                uc <- getLine 
                conteudoAlunos <- readFile "listaalunos.txt"
                filtro_uc uc (lines conteudo)
                filtro_ano_nome uc (lines conteudoAlunos)
            
     filtro_uc :: String -> [String] -> IO()
     filtro_uc uc [] = return()                                                              --Do Ficheiro das Uc seleciona a Uc que recebe como argumento
     filtro_uc uc(linha:linhas) = do
                    if head(words linha) == uc then 
                         if last(words linha) == "funcional" then putStrLn ((words linha !! 2) ++ " " ++ (last(words linha)))
                              else putStrLn (words linha !! 2)
                        else filtro_uc uc linhas

     filtro_ano_nome :: String -> [String] -> IO ()
     filtro_ano_nome uc [] = return ()                                                                   --Filtra a uma Determinada Uc por ano
     filtro_ano_nome uc (li : lis) = do
          let ws = words li
          if length ws >= 4 && ws !! 1 == uc
           then do
               putStrLn (ws !! 2 ++ " " ++ ws !! 3)
               filtro_ano_nome uc lis
          else
               filtro_ano_nome uc lis

     le_inscricoes :: IO()
     le_inscricoes = do                                                                                  --Le o ficheiro das inscriçoes e pede um nome e chama outra funcao
             putStrLn "Insira um nome de um aluno"
             nome <- getLine 
             inscricoes <- readFile "listaalunos.txt"
             show_name_al nome (lines inscricoes)
             
   
     filtra_al :: String -> [String] -> IO()
     filtra_al al [] = return ()                                                                         --Dado uma lista encontra o al nessa lista e chama uma função com o Uc do aluno correspondente
     filtra_al al (lis:list) = do
                                            if head(words lis) == al then do
                                                                             show_uc_aluno (last (words lis)) 
                                                                             filtra_al al list

                                            else filtra_al al list
 
     show_uc_aluno :: String -> IO()
     show_uc_aluno uc = do                                                                               --Le um ficheiro e chama uma função
                         ucs_file <- readFile "ucs.txt"
                         filtro_uc uc (lines ucs_file)

     tarefa1 :: IO a
     tarefa1 = do                                                                                        -- Le o ficheiro e chama um função
               conteudo <- readFile "ucs.txt"
               show_uc (lines conteudo)


     show_uc :: [String] -> IO a
     show_uc (linha:linhas) = do                                                                         --Mostra A primeira Uc e chama uma função que vai retornar todos os alunos que esta nessa Uc
                              if last(words linha) == "funcional" then putStrLn ((words linha !! 2) ++ " " ++ (last(words linha)))
                                   else putStrLn (words linha !! 2)
                              show_nome (head(words linha))
                              show_uc linhas
                              

     show_nome :: String -> IO ()
     show_nome uc = do                                                                                   --Le um ficheiro e chama uma função que vai filtra o nome com a uc pretendida
                    conteudoAlunos2 <- readFile "listaalunos.txt"
                    filtro_ano_nome uc (lines(conteudoAlunos2))

     tarefa2 :: IO a
     tarefa2 = do
               conteudoAlunos3 <- readFile "listaalunos.txt"
               prato <- readFile "inscricoes.txt"
               show_name (lines conteudoAlunos3)
               

     show_name :: [String] -> IO a
     show_name (name:names)= do                                                                         --le a lista de alunos e chama duas funções para filtrar e imprimir os nomes
                             putStrLn((words name !! 2) ++ " " ++ (words name !! 3)) 
                             insclis <- readFile "inscricoes.txt"
                             filtra_al (head (words name)) (lines insclis)
                             show_name names

     show_name_al :: String -> [String] -> IO()
     show_name_al nome [] = error "Aluno não existe"                                                     --Verifica qual o aluno que instroduzimos e com o Al de esse aluno chama outra função para saber em que Uc's esta inscrito
     show_name_al nome (incri:incricos) = do
                                        if ((words incri) !! 2) == nome then do
                                                                                                         putStrLn (((words incri) !! 2) ++ " " ++ ((words incri) !! 3))
                                                                                                         inscricoes <- readFile "inscricoes.txt"
                                                                                                         filtra_al (head (words incri)) (lines inscricoes)
                                        else
                                             show_name_al nome incricos