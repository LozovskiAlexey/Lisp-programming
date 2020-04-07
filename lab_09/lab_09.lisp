; Написать предикат set-equal, который возвращает t, если два его множество-аргумента 
; содержат одни и те же элементы, порядок которых не имеет значения.

; поиск элемента в списке
(defun find-equal (elem list)
    (reduce 
        (lambda (result tmp)
            (if (equal tmp elem)
                tmp
                result
            )
        ) list :initial-value 
    )
)

(find-equal 5 '(1 2 3 4 5))
(find-equal 6 '(1 2 3 4 5))

; рекурсивная проверка наличия элемента в списке 
(defun find-equal (elem list)
    (cond 
        ((equal elem (car list))  t)
        ((null (cdr list))  Nil)
        (t  (find-equal elem (cdr list)))
    )
)

(find-equal 5 '(1 2 3 4 5))
(find-equal 6 '(1 2 3 4 5))

; проверка содержания всех элементов списка1 в списке2
(defun cmp-lists (list1 list2)
    (reduce 
        (lambda (result elem)
            (eq result (eq (find-equal elem list2) t))
        ) list1 :initial-value t
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



; Напишите функцию, которая умножает на заданное число-аргумент все числа
; из заданного списка-аргумента, когда
; a) все элементы списка --- числа,
; 6) элементы списка -- любые объекты.
