(define sub
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n)
    (+ n 1)))

(define number-list?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((not (pair? lat)) #t)
      (else
        (and (number? (car lat)) (number-list? (cdr lat)))))))


;; Key-value 形式リストからデータを取得する
;; #?=(memo-pick-value '(0 1) '(((0 1) #t) ((0 2) #f)))
;;     >>  #t
(define memo-pick-value
  (lambda (key lat)
    (cond
      ((null? lat) '())
      ((equal? key (caar lat)) (cadar lat))
      (else
        (memo-pick-value key (cdr lat))))))

;; Key-value 形式リストにデータを追加する
;; #?=(memo-push-value '(((0 1) #t) ((0 2) #f)) '((0 3) #f))
;;     >>  (((0 1) #t) ((0 2) #f) ((0 3) #f))
(define memo-push-value
  (lambda (lat value)
    (cond
      ((null? lat) (cons value '()))
      (else
        (cons (car lat) (memo-push-value (cdr lat) value))))))


;;; 座標情報であるかどうか
(define board-point?
  (lambda (pos)
    (cond
      ((null? pos) #f)
      (else
        (and
          (= 1 (lat-size pos))
          (number-list? pos)
          (not (negative? (car pos)))
          (not (negative? (cadr pos))))))))

;;;要求されたサイズでボード表現列を初期化する
(define mk-line
  (lambda (n)
    (cond
      ((zero? n) '())
      (else
        (cons
          -1
          (mk-line (sub n)))))))

;;;ボードを要求されたサイズで初期化する    
(define mk-board
  (lambda (col row)
    (cond
      ((zero? row) '())
      (else
        (cons
          (mk-line col)
          (mk-board col (sub row)))))))

;;; 配列内のデータをピックアップする
(define pick
  (lambda (lat n)
    (cond
      ((null? lat) '())
      ((zero? n) (car lat))
      (else
        (pick (cdr lat) (sub n))))))        

;;; 指定した座標の情報を取得する
(define pick-board
  (lambda (pos board)
    (cond
      ((null? board) '())
      (else
        (pick (pick board (cadr pos)) (car pos))))))

;;; 指定した座標にデータを挿入する
(define update-board
  (lambda (board col row n)
    (cond
      ((zero? row)
        (cons
          (update-list (car board) col n)
          (cdr board)))
      (else
        (cons (car board)
          (update-board (cdr board) col (sub row) n))))))

(define update-list
  (lambda (lat col n)
    (cond
      ((zero? col) (cons n (cdr lat)))
      (else
        (cons (car lat)
          (update-list (cdr lat) (sub col) n))))))

;;; リストのサイズを取得する
(define lat-size
  (lambda (lat)
    (cond
      ((not (pair? lat)) 0)
      ((null? (cdr lat)) 0)
      (else
        (add (lat-size (cdr lat)))))))

;;; プレイヤー情報を文字表現にする
(define player-letter
  (lambda (n)
    (cond
      ((not (number? n)) "?")
      (else
        (cond
          ((= -1 n) "+")
          ((= 0 n) "O")
          ((= 1 n) "@")
          (else "?"))))))

;;; 座標のshift
(define shift-point-down
  (lambda (pos)
    (cons (car pos) (cons (add (cadr pos)) ()))))

(define shift-point-up
  (lambda (pos)
    (cons (car pos) (cons (sub (cadr pos)) ()))))

(define shift-point-right
  (lambda (pos)
    (cons (add (car pos)) (cons (cadr pos) ()))))

(define shift-point-left
  (lambda (pos)
    (cons (sub (car pos)) (cons (cadr pos) ()))))

;;; ボードを標準出力に出力する
(define print-board
  (lambda (board)
    (cond
      ((null? board) (print ""))
      (else
        (print (draw-line-string (car board)))
        (print-board (cdr board))))))

;;; ボードの一行分を表現した文字列を返却する
(define draw-line-string
  (lambda (lat)
    (cond
      ((null? lat) "\n")
      (else
        (string-append (player-letter (car lat)) " " (draw-line-string (cdr lat)))))))

;;; 手番を管理する
(define player-turn
  (lambda (p)
    (cond
      ((= p 0) 1)
      (else 0))))

;;; 石を置く***
(define stone-put
  (lambda (p input)
    (update-board *board* (car input) (cadr input) p)))


;;; 自身の周りに呼吸点が存在するか。存在すれば #t
;;; 一度調査した場所を二度と調査しないための変数*memo*クリアをここで行う
;;; この辺は多分メモ化できそう
(define *memo* '())
(define kokyuu?
  (lambda (pos player)
    (set! *memo* (memo-push-value '() (cons pos (cons #t '()))))
    (kokyuu-memo? pos player)))

;;; 自身の周りに呼吸点が存在するか。存在すれば #t
(define kokyuu-memo?
  (lambda (pos player)
     (cond
        ((or
          (kokyuu-cell? player (shift-point-up pos))
          (kokyuu-cell? player (shift-point-right pos))
          (kokyuu-cell? player (shift-point-down pos))
          (kokyuu-cell? player (shift-point-left pos))) #t)
        (else #f))))

;;; 呼吸点（空白）かどうか。空白の場合 #t。
;;; 自石のばあい再帰的に呼吸点を探す。
(define kokyuu-cell?
  (lambda (player pos)
    (cond
      ((eq? (memo-pick-value pos *memo*) #t) #f)
      (else
        (set! *memo* (memo-push-value *memo* (cons pos (cons #t '()))))
        (cond
          ((not (board-point? pos)) #f) ;; 位置が盤面を超えている場合
          ((= -1 (pick-board pos *board*)) #t)
          ((= player (pick-board pos *board*)) (kokyuu-memo? pos player) )
          (else #f))))))

;;; *********************************************************
(define *player* 0)
(define *board-size* 9)
(define *board* (mk-board *board-size* *board-size*))

;; test1コマ配置
(set! *board* (stone-put 1 '(1 0)))
(set! *board* (stone-put 1 '(2 1)))
(set! *board* (stone-put 1 '(0 1)))
(set! *board* (stone-put 1 '(1 2)))

;; test2コマ配置
(set! *board* (stone-put 1 '(0 4)))
(set! *board* (stone-put 0 '(1 5)))
(set! *board* (stone-put 1 '(2 5)))
(set! *board* (stone-put 1 '(1 4)))
(set! *board* (stone-put 1 '(1 6)))
(set! *board* (stone-put 0 '(0 6)))
(set! *board* (stone-put 1 '(0 7)))

;; 現在の盤面を出力する
(print-board *board*)

;; ボードの任意の位置にある石情報を取得するテスト
;;#?=(pick-board '(0 0) *board*)
;;#?=(pick-board '(0 1) *board*)
;;#?=(pick-board '(0 2) *board*)
;;#?=(pick-board '(0 3) *board*)
;;#?=(pick-board '(0 4) *board*)
;;#?=(pick-board '(0 5) *board*)
;;#?=(pick-board '(0 6) *board*)
;;#?=(pick-board '(0 7) *board*)
;;#?=(pick-board '(0 8) *board*)

;;; 呼吸点が存在するかチェックするユーティリティのテスト
#?=(kokyuu? '(1 1) *player*)
#?=(kokyuu? '(0 5) *player*)
#?=(kokyuu? '(3 3) *player*)





;;    (print-board *board*)
;;
;;    (print "please input.")
;;    (set! *board* (stone-put *player* (read)))
;;    (set! *player* (player-turn *player*))
;;    (print-board *board*)



