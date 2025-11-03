(ns integrador.core)

(defn cadastrar-alunos []
  (loop [alunos []]
    (println "\nDigite o nome do aluno:")
    (let [nome (read-line)]
      (if (empty? nome)
        alunos
        (do
          (println "Digite a nota do aluno:")
          (let [nota (Double/parseDouble (read-line))
                aluno {:nome nome :nota nota}]
            (recur (conj alunos aluno))))))))

(defn relatorio-notas [alunos]
  (if (seq alunos)
    (do
      (println "\n=== RELATORIO DE NOTAS ===")
      (let [alunos-com-status (map (fn [a]
                                     (assoc a :status (if (>= (:nota a) 6)
                                                        "Aprovado"
                                                        "Reprovado")))
                                   alunos)
            aprovados (filter #(= (:status %) "Aprovado") alunos-com-status)
            media (/ (reduce + (map :nota alunos)) (count alunos))]
        (println "\nAlunos cadastrados:")
        (doseq [a alunos-com-status]
          (println (:nome a) "-" (:nota a) "-" (:status a)))
        (println "\nAprovados:")
        (doseq [a aprovados]
          (println (:nome a) "-" (:nota a)))
        (println "\nMedia geral da turma:" media)
        alunos))
    (println "Nenhum aluno cadastrado")))

(defn estatisticas-gerais [alunos]
  (if (seq alunos)
    (let [total (count alunos)
          aprovados (count (filter #(>= (:nota %) 6) alunos))
          reprovados (- total aprovados)
          notas (map :nota alunos)
          maior (apply max notas)
          menor (apply min notas)
          media (/ (reduce + notas) total)]
      (println "\n=== ESTATISTICAS GERAIS ===")
      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" maior)
      (println "Menor nota:" menor)
      (println "Media geral da turma:" media)
      alunos)
    (println "Nenhum aluno cadastrado.")))

(defn buscar-aluno [alunos]
  (if (seq alunos)
    (do
      (println "\nDigite o nome do aluno que deseja buscar:")
      (let [nome (read-line)
            aluno (first (filter #(= (:nome %) nome) alunos))]
        (if aluno
          (let [status (if (>= (:nota aluno) 6) "Aprovado" "Reprovado")]
            (println "\nAluno encontrado:")
            (println "Nome:" (:nome aluno))
            (println "Nota:" (:nota aluno))
            (println "Status:" status))
          (println "Aluno nao encontrado"))))
    (println "Nenhum aluno cadastrado")))

(defn -main []
  (loop [alunos []]
    (println "\n=== MENU PRINCIPAL ===")
    (println "1 - Cadastrar Alunos")
    (println "2 - Relatorio de Notas")
    (println "3 - Estatisticas Gerais")
    (println "4 - Buscar Aluno pelo Nome")
    (println "0 - Sair")
    (print "Escolha uma opcao: ")
    (flush)
    (let [opcao (read-line)]
      (cond
        (= opcao "1") (recur (cadastrar-alunos))
        (= opcao "2") (do (relatorio-notas alunos) (recur alunos))
        (= opcao "3") (do (estatisticas-gerais alunos) (recur alunos))
        (= opcao "4") (do (buscar-aluno alunos) (recur alunos))
        (= opcao "0") (println "Saindo do sistema")
        :else (do (println "Opcao invalida.") (recur alunos))))))

