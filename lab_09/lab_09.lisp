; Написать предикат set-equal, который возвращает t, если два его множество-аргумента 
; содержат одни и те же элементы, порядок которых не имеет значения.


; функциональный поиск элемента в списке
(defun find-equal (elem list)
    (reduce 
        (lambda (result tmp)
            (if (equal tmp elem)
                T
                result
            )
        ) list :initial-value nil
    )
)

(find-equal 5 '(1 2 3 4 5))
(find-equal 6 '(1 2 3 4 5))


; рекурсивный поиск элемента в списке 
(defun find-equal (elem list)
    (cond 
        ((equal elem (car list))  t)  ; если первый элемент списка равен искомому элементу вернем true 
        ((null (cdr list))  Nil)      ; если встречен последний элемент списка вернем nil 
        (t  (find-equal elem (cdr list)))   ; ищем элемент в хвосте списка
    )
)

(find-equal 5 '(1 2 3 4 5))
(find-equal 6 '(1 2 3 4 5))


; функциональная проверка содержания всех элементов списка1 в списке2
; устанавливаем в result значение t. Для ищем каждый элемент списка1 в списке2 
; последовательно, с помощью функции find-elem(если найдено - вернет t, иначе nil). 
; дальше смотрим, если элемент не найден (find-equal -> nil или result -> nil функция вернет nil)
(defun cmp-lists (list1 list2)
    (reduce 
        (lambda (result elem)
            (and result (find-equal elem list2))  
        ) list1 :initial-value t
    )
)

(cmp-lists '(2 3 4) '(2 3 4))
(cmp-lists '(2 3 4) '(2 5 4))


; рекурсивная проверка содержания всех элементов списка1 в списке2
(defun cmp-lists (list1 list2)
    (cond 
        ((null list1)  T)                                               ; если дошли до конца списка - вернем T 
        ((find-equal (car list1) list2)  (cmp-lists (cdr list1) list2)) ; если нашли текущий элемент в другом списке, ищем следующий
        (T nil)                                                         ; если элемент не найден и не конец списка - nil 
    )
)

(cmp-lists '(2 3 4) '(2 3 4))
(cmp-lists '(2 3 4) '(2 5 4))


(defun set-equal(list1 list2)
    (if (equal (length list1) (length list2))
        (cmp-lists list1 list2)
        Nil
    )
)

(set-equal '(1 2 3 4) '(2 3 4 5))
(set-equal '(1 2 3 4) '(1 2 3))
(set-equal '(1 2 3 4) '(1 2 3 4))


; Напишите необходимые функции, которые обрабатывают таблицу из точечных пар:
; (страна. столица), и возвращают по стране - столицу, а по столице - страну.


; генерация таблицы из двух списков
(defun generate-table(list1 list2)
    (mapcar #'cons list1 list2)
)

(generate-table '(Russia Italy Germany Spain) '(Moscow Rome Berlin Madrid))

; Функционально возвращает по стране столицу 
(defun get-capital (country list)
    (reduce 
        (lambda (result tmp)               ; функция, tmp - текущий элемент списка list 
            (if (equal (car tmp) country)  ; если голова текущего элемента - искомая страна
                (cdr tmp)                  ; возвращаем хвост (столицу)
                result                     ; иначе вернем значение, на предыдущей итерации 
            )
        ) list :initial-value Nil          ; флаг задает начальное значение для переменной result 
    )
)

; Рекурсивно возвращает по стране столицу
(defun get-capital (country list)
    (cond 
        ((null list) Nil)                                    ; если прошли весь список
        ((equal (car (car list)) country)  (cdr (car list))) ; Если нашли страну - вернуть столицу
        ( t (get-capital country (cdr list)))                ; переходим к следующему элементу (рекурсивно)
    )
)

(get-capital 'Germany (generate-table '(Russia Italy Germany Spain) '(Moscow Rome Berlin Madrid)))
(get-capital 'Norway (generate-table '(Russia Italy Germany Spain) '(Moscow Rome Berlin Madrid))) 

; функционально возвращает по столице страну
(defun get-country (capital list)
   (reduce 
        (lambda (result tmp)               ; функция, tmp - текущий элемент списка list 
            (if (equal (cdr tmp) capital)  ; если хвост текущего элемента - искомая столица
                (car tmp)                  ; возвращаем голову (страну)
                result                     ; иначе вернем значение, на предыдущей итерации 
            )
        ) list :initial-value Nil          ; флаг задает начальное значение для переменной result 
    )
)

; рекурсивно возвращает по столице страну 
(defun get-сountry (capital list)
    (cond 
        ((null list) Nil)                                    ; если прошли весь список
        ((equal (cdr (car list)) capital)  (car (car list))) ; Если нашли столицу - вернуть страну
        ( t (get-country country (cdr list)))                ; переходим к следующему элементу (рекурсивно)
    )
)

(get-country 'Germany (generate-table '(Russia Italy Germany Spain) '(Moscow Rome Berlin Madrid)))
(get-country 'Rome (generate-table '(Russia Italy Germany Spain) '(Moscow Rome Berlin Madrid)))

; Напишите функцию, которая умножает на заданное число-аргумент все числа
; из заданного списка-аргумента, когда
; a) все элементы списка -- числа,
; 6) элементы списка -- любые объекты.

; функциональная реализация
(defun mult(digit list)
    (mapcar 
        (lambda (elem) 
            (* digit elem)
        ) list)
)

(mult 5 '(1 2 3 4 5))

(defun super-mult (digit list)
    (mapcar 
        (lambda (elem)
            (cond 
                ((listp elem) (super-mult digit elem))
                ((numberp elem)  (* elem digit))
                (t elem)
            )
        )
    )
)

; рекурсивная реализация 
(defun mult (digit list)
    (cond 
        ((null list) nil)
        (t (cons (* (car list) digit) (mult digit (cdr list))))
    )
)

(defun super-mult (digit list)
    (cond 
        (  (null         list )   nil                                                               )   
        (  (numberp (car list))  (cons (* (car list) digit) (super-mult digit (cdr list)))          )
        (  (listp   (car list))  (cons (super-mult digit (car list)) (super-mult digit (cdr list))) )
        (  t                     (cons (car list) (super-mult digit (cdr list)))                    )
    )
)