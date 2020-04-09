; Дан смешанный, структурированный список (т.е. любой элемент списка может быть: символ, число или список).
; Найти сумму четных чисел на всех уровнях заданного списка, лежащих в заданном интервале [a, b].
; Добавить найденную сумму к исходному списку, в качестве К-ого элемента верхнего уровня или в конец. Если чисел нет, сообщить об этом.


; проверка элемента на четность
(defun is-odd(elem)
    (eq (mod elem 2) 0)  ; проверка на четность
)

(is-odd 6)   ;-> T 
(is-odd 7)   ;-> NIL

; проверяет принадлежность элемента границам
(defun elem-in (lb rb elem)
    (and (>= elem lb) (<= elem rb))
)

(elem-in 10 20 5)
(elem-in 10 20 15)


; добавляет число если оно в границах и четное
(defun check-elem(lb rb elem)
    (cond 
        ((and (is-odd elem) (elem-in lb rb elem)) elem)
        (t 0)
    )
)

(check-elem 5 20 6) ;->0
(check-elem 5 20 7) ;->

; главная функция программы
(defun odd-sum (lb rb list)
    (reduce 
        (lambda (sum elem)
            (cond 
                ((null elem) sum)
                ((numberp elem)  (+ (check-elem lb rb elem) sum)) ; если элемент - четное число, добавить к сумме 
                ((listp elem)    (+ (odd-sum lb rb elem) sum)) ; если элемент список - применить на нем функцию
                (t sum)
            )
        ) list :initial-value 0
    )
)

; добавляет сумму в конец списка
(defun add-odd-sum-to-end (lb rb list)
    (reduce 
        (lambda (elem result-list)
            (cons elem result-list)
        ) list :initial-value (cons (odd-sum lb rb list) nil) :from-end t
    )
)

; (ADD-ODD-SUM-TO-END 2 7 '(1 2 4 5 7 10)) ; -> (1 2 4 5 7 10 6)

; хвостовая рекурсия, lb rb - границы
; list список-аргумент 
; sum - накапливаемая сумма
(defun add-sum-rec (lb rb list sum)
    (cond 
        ((null list) sum)                                                                           
        ((numberp (car list)) (add-sum-rec lb rb (cdr list) (+ (check-elem lb rb (car list)) sum))) ; если число прибавляем к сумма 
        ((listp (car list))   (add-sum-rec lb rb (cdr list) (+ (add-sum-rec lb rb (car list) sum) sum))) ; если элемент список - считаем сумму четных элементов внутри и прибавляем ее к сумме
        (t                    (add-sum-rec lb rb (cdr list) sum)) 
    )
)

(add-sum-rec 3 7 '(2 3 4 5 6 7 8 9) 0)

; рекурсивная версия
(defun odd-sum-rec-shell (lb rb list)
    (append list (cons (add-sum-rec lb rb list 0) nil))
)

(ODD-SUM-REC-SHELL 3 7 '(2 3 4 5 6 7 8 9)) ; -> (2 3 4 5 6 7 8 9 10)


; Преобразовать исходный список в одноуровневый, сохранив порядок, но удалив числа (для max-х баллов).
(defun structed_to_one_level (lst res)
    (cond 
        ((null lst)                  res)       
        ((listp (car lst))          (structed_to_one_level (cdr lst) (append (structed_to_one_level (car lst) Nil) res)))    
        ((not (numberp (car lst)))  (structed_to_one_level (cdr lst) (cons (car lst) res)))
        (T                          (structed_to_one_level (cdr lst) res))
    )
)

(reverse (structed_to_one_level '(1 2 a (b) 3) 5))
