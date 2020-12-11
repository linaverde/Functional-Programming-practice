;В игре участвуют два игрока. Множество карт игрока представлено списком. 
;Элемент списка имеет следующую структуру: название масти, название карты. 
;Также задан второй список, в котором каждой карте колоды сопоставляется «стоимость» - некоторое число. 
;Написать функцию, которая по всем картам заданной масти определяет, у кого из игроков стоимость карт этой масти максимальная. 
;Написать функцию, которая определяет, какие карты остались в колоде.


;суммирует стоиости карт в руке
(defun sum (price res)
    (COND
        ((null price) res)
        (T (sum (CDR price) (+ (CAR price) res)))
    )
)

;заменяет карты в колоде на список стоимостей
(defun change (cards prices res)
    (COND
        ((null cards) res)
        (T (change (CDR cards) prices (CONS (get_price (CAR cards) prices) res) ))
    )

)

;удаляет тип карты
(defun del_type (hand res)
    (COND
        ((null hand) res)
        (T (del_type (CDR hand) (CONS (CAR(CDR(CAR hand))) res)) )
    )
)

;удаляет все карты кроме карт указанного типа
(defun get_cards (hand type res)
    (COND
        ((null hand) res)
        ((equal (CAR (CAR hand)) type) (get_cards (CDR hand) type (CONS (CAR hand) res)) ) 
        (T (get_cards (CDR hand) type res))
    )
)

;Возыращает стоимость карты по карте в руке
(defun get_price (card prices)
    (COND 
        ((null prices) 0)
        ((equal card (CAR (CAR prices)))  (CAR(CDR(CAR prices)))  )
        (T (get_price card (CDR prices)))
        
    )
)

;высчитывает стоимость карт на руках у игрока
(defun player_result (player ctype prices)
    (sum(change (del_type(get_cards player ctype NIL) NIL) prices NIL) 0)
)

;сравнивает суммы стоимостей карт и выводит кто победил
(defun who_win (player1 player2 ctype prices)
    (COND
        ( ( > (sum(change (del_type(get_cards player1 ctype NIL) NIL) prices NIL) 0) (sum(change (del_type(get_cards player2 ctype NIL) NIL) prices NIL) 0) )  (print "Первый игрок победил") )
        ( ( < (sum(change (del_type(get_cards player1 ctype NIL) NIL) prices NIL) 0) (sum(change (del_type(get_cards player2 ctype NIL) NIL) prices NIL) 0))  (print "Второй игрок победил") )
        (T (print "Ничья"))
    )
)

;удаляет цену карты из определения карт в колоде
(defun del_price (cards res)
    (COND 
        ((null cards) res)
        (T (del_price (CDR cards) (CONS (CONS(CAR (CAR cards)) (CONS (CAR(CDR(CAR cards))) nil))   res  )))
    
    )
)

; удаляет карту из списка
(defun del_card (cards c res)
    (COND
        ((null cards) res)
        ((equal (CAR cards) c) (del_card (CDR cards) c res) )
        (T (del_card (CDR cards) c (CONS (CAR cards) res)))
    )
) 

;совмещает два списка карт у игроков в руках в один список
(defun concat (hand1 hand2)
    (COND
        ((null hand2) hand1)
        (T (concat (CONS (CAR hand2) hand1) (CDR hand2)))
    )

)

;возвращает карты, оставшиеся в колоде
(defun left (cards hand)
    (COND
        ((null hand) cards)
        (T (left (del_card cards (CAR hand) nil) (CDR hand)))
    )
)

(let
    ( 
        (prices '(("Треф" "А" 1) ("Треф" "Б" 2) ("Треф" "В" 3) ("Треф" "Г" 4) ("Треф" "Д" 5) ("Буби" "А" 1) ("Буби" "Б" 2) ("Буби" "В" 3) ("Буби" "Г" 4) ("Буби" "Д" 5) ))
        (player1 '(("Треф" "А") ("Буби" "Г") ("Треф" "Б")))
        (player2 '(("Треф" "Д") ("Буби" "Б") ("Буби" "А")))
        (ctype "Треф" )
    )
    
   (who_win player1 player2 ctype prices)
   (print "Оставшиеся карты в колоде")
   (print (left (del_price prices nil) (concat player1 player2)))
) 