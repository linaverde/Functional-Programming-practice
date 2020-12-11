;Про каждого студента известная следующая информация: какой предмет сдавал, 
;каковы были результаты (оценки от 2 до 5), когда происходил экзамен или пересдача. 
;Написать функцию, которая по заданным предметам упорядочивает список студентов по возрастанию количества пересдач.


;функция для добавления элемента в уже отсортированный список на своё место
(defun insert (elem head tail) 
    (COND 
        ;;; Если элемент пустой - значит мы его уже добавили. Объединяем списки
        ((null elem)
            (COND
                ;;; Если голова пустая - закончили, возвращаем список
                ((null head) tail)
                ;;; Иначе берем элемент из головы и присоеденяем к хвосту
                (T (insert nil (CDR head) (CONS (CAR head) tail)))
            )
        )
        ;;; Если хвост пустой и голова пустая - добавляем в список и возвращаем хвост - список из одного элемента
        ((and (null tail) (null head))  (CONS elem nil))
        ;;; Если элемент больше первого элемента хвоста - перекладываем голову хвоста в голову головы
        ((> (CAR(CDR elem))  (CAR(CDR(CAR tail))))  (insert elem (CONS (CAR tail) head) (CDR tail)))
        ;;; Если элемент меньше первого элемента хвоста - значит он нашел своё место. Перекидываем его в голову
        ((< (CAR(CDR elem))  (CAR(CDR(CAR tail)))) (insert nil (CONS elem head) tail) )
        ((equal (CAR(CDR elem))  (CAR(CDR(CAR tail)))) (insert nil (CONS elem head) tail) )
        
    )
    
)

;сортировка списка (из списка по очереди добавляет элементы в новый заранее отсортированный список)
(defun sorting (students res)
    (COND 
        ((null students) res)
        (T (sorting (CDR students) (insert (CAR students) nil res)) )
    )

)

;заменяет список экзаменов и пересдач у студента на количество пересдач
(defun change (students discipline res)
    (COND 
        ((null students) res)
        (T (change (CDR students) discipline (CONS (LIST (CAR(CAR students)) (re_count (CAR(CDR(CAR students))) discipline 0))  res)))
    )

)

;подсчитывает количество пересдач студента
(defun re_count (exams discipline count)
    (COND
        ((null exams) count)
        ((and (equal (CAR (CAR exams)) "Пересдача") (equal (CAR(CDR(CAR exams))) discipline))  (re_count (CDR exams) discipline (+ count 1)) )
        (T (re_count (CDR exams) discipline count))
    )
)

(let
    ( 
        (students '(
        ("Иванов" (("Экзамен" "История" 5 "28.10") ("Экзамен" "Математика" 2 "28.10") ("Пересдача" "Математика" 3 "28.10")))  
        ("Петров" (("Экзамен" "История" 5 "28.10") ("Экзамен" "Математика" 5 "28.10") ("Экзамен" "Программирование" 4 "28.10")))
        ("Двоечник" (("Пересдача" "История" 4 "28.10") ("Пересдача" "Математика" 3 "28.10") ("Экзамен" "Программирование" 4 "28.10") ("Пересдача" "Математика" 3 "28.10")))
        ))
        
    )
    
    (print (sorting (change students "Математика" nil) nil))

) 