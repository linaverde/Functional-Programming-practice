(let
    ((l '(1 2 3 4 5 4 2))
    (num 5))
    
    ;сумма эл списка с фиксированным числом
    (defun plus (arg)
        (+ arg num))
    
    ;умножение элемента списка на фиксированное число
    (defun mult (arg)
        (* arg num))
        
    ;создание списка пар элементов с фиксированным числом    
    (defun pair (arg)
        (list arg num))
        
    ;создание списка списков
    (defun make_list (arg)
        (list arg)
    )
    
    ;создает список из элемента и количества его вхождений
    (defun elem_count (arg)
        (defun counter (l elem count)
            (COND
                ((NULL l) count)
                ((= (CAR l) elem) (counter (CDR l) elem (+ 1 count)))
                (T (counter (CDR l) elem count))
            )
        )
        
        (list arg (counter l arg 0))
    )
    
    ;список из разностей соседних элементов
    (defun minus_couple (arg1 arg2)
        (list (- arg1 arg2))
    )
    
    ;список из сумм соседних элементов
    (defun plus_couple (arg1 arg2)
        (list (+ arg1 arg2))
    )
    
    ;функция высшего порядка для функций от двух аргументов
    (defun ho_two (fun l)
        (COND
            ((null (CDR l)) nil)
            (T (CONS (funcall fun (CAR l) (CAR(CDR l))) (ho_two fun (CDR l))) )
        )
    )
    
    ;функция высшего порядка для функций из одного аргумента
    (defun high_order_call (fun l)
    (COND
        ((null l) nil)
        (T (CONS (funcall fun (CAR l)) (high_order_call fun (CDR l))))
    ))
    
    
(print(high_order_call 'plus l))
(print(high_order_call 'mult l))
(print(high_order_call 'pair l))
(print(high_order_call 'make_list l))
(print(high_order_call 'elem_count l))
(print(ho_two 'minus_couple l))
(print(ho_two 'plus_couple l))

)