
;Написать функцию, которая по списку вида:
;- фамилия
;- улица
;- номер дома
;Сформирует список однофамильцев, проживающих на заданной улице.


;;; Удаляет все указанные элементы из списка
(defun del (l elem res)
    (COND
        ((NULL l) res)
        (T (COND 
            ((equal (CAR l) elem) (del (CDR l) elem res))
            (T (del (CDR l) elem (CONS (CAR l) res)))
            )
        )
    )
)


;;; Проверяет, повторяется ли элемент в списке. All - список всех элементов, изменяется только при удалении. Res - список повторяющихся элементов. l - проходит по списку.
(defun same (all l res)
    (COND
    ((NULL all) res)
    (T (COND
        ;Если дошли до конца списка, то запускаем эту функцию со следующим элементом из ALL
        ((NULL l) (same (CDR all) (CDR all) res))
        ;Если голова l совпадает с головой ALL, то вносим в res эту фамилию и удалем все ее вхождения из ALL
        ((equal (CAR all) (CAR l)) (same (del all (CAR L) NIL) (CDR (del all (CAR L) NIL)) (CONS (CAR all) res)))
        ;Если голова не совпадает, то запускаем эту функцию со след элементом l
        (T (same all (CDR l) res))
    ))
    )
)




;;; Превращает список элементов в список фамилий
(defun list_names (l names)
    (COND
        ((NULL l) names)
        (T (list_names (CDR l) (CONS (CAR (CAR l)) names) ))
    )
)


;;; Создает список элементов с заданной улицей
(defun list_street (street l res)
    (COND
        ((NULL l) res)
        (T (COND 
            (
                (check_street street (get_street (CAR l)))  
                (list_street street (CDR l) (CONS (CAR l) res)) 
            )
            (T (list_street street (CDR l) res))
            )   
        )
    )
)

;;; Принимает название улицы и элемент списка. Сравнивает название улицы, возвращает элемент при совпадении и NIL при разных улицах
(defun check_street (street elem)
    (COND 
    ((equal street elem) T) 
    (T NIL))
)

;;; Принимает название улицы и элемент списка. Сравнивает название улицы, возвращает элемент при совпадении и NIL при разных улицах
(defun get_street (elem)
    (CAR(CDR elem))
)

;;;Формирует представление ответа
(defun get_ans (l street)
    (print
        (same 
            (list_names (list_street street l NIL) NIL)
            (CDR (list_names (list_street street '(("Иванов" "Улица" 2) ("Петров" "Чугуевка" 3) ("Спицин" "Улица" 15) ("Спицин" "Улица" 15)) NIL) NIL))
            NIL
        )
    )
)

(get_ans '(("Иванов" "Улица" 2) ("Петров" "Чугуевка" 3) ("Спицин" "Улица" 15) ("Спицин" "Улица" 15)) "Улица")
