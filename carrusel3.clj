
;Funcion para leer el archivo por default
  (defn leer-archivo [carrusel]
    (with-open [archivo (clojure.java.io/reader carrusel)]
      (read-string (slurp archivo))))

;Funcion oara obtener el producto actual
  (defn infproducto [columna lista]
    (cond (= columna (first (first lista))) (first lista)
          :else (infproducto columna (rest lista))))

  (defn info-producto [carrusel]
    (println (infproducto (first (leer-archivo carrusel)) (last (first (last (leer-archivo carrusel)))))))
;Funciones para rotar el carrusel tanto arriba como abajo, cuentan con auxiliares para esto y se reescribe en el programa
  (defn abajo [liste]
    (cons (last liste) (butlast liste)))

  (defn abajon [carrusel]
    (spit carrusel
          (list (first (leer-archivo carrusel)) (abajo (last (leer-archivo carrusel)))))
    (info-producto carrusel))

  (defn arriba [liste]
    (concat (rest liste) (list (first liste))))

  (defn arriban [carrusel]
    (spit carrusel
          (list (first (leer-archivo carrusel)) (arriba (last (leer-archivo carrusel)))))
    (info-producto carrusel))

;Funciones para dar vuelta al carrusel a la derecha o izquierda y muestran si se puedo realizar la accion, al igual que se actualiza el archivo de origen
  (defn derechar [liste posicion]
    (cond (empty? liste) false
          (= (first (first liste)) posicion) (cond (empty? (rest liste)) false
                                                   :else (first (rest liste)))
          :else (derechar (rest liste) posicion)))

  (defn derechon [carrusel]
    (cond (= (derechar (last (first (first (rest (leer-archivo carrusel))))) (first (leer-archivo carrusel))) false) (println "Movimiento invalido")
          :else (do
                  (println (derechar (last (first (first (rest (leer-archivo carrusel))))) (first (leer-archivo carrusel))))

                  (spit carrusel (list (+ 1 (first (leer-archivo carrusel))) (last (leer-archivo carrusel)))))))

  (defn izquierdar [liste posicion]
    (cond (= 1 (first (last liste))) false
          (= posicion (first (last liste))) (last (butlast liste))
          :else (izquierdar (butlast liste) posicion)))

  (defn izquierdon [carrusel]
    (cond (= (izquierdar (last (first (first (rest (leer-archivo carrusel))))) (first (leer-archivo carrusel))) false) (println "Movimiento invalido")
          :else (do
                  (println (izquierdar (last (first (first (rest (leer-archivo carrusel))))) (first (leer-archivo carrusel))))
                  (spit carrusel (list (- (first (leer-archivo carrusel)) 1) (last (leer-archivo carrusel)))))))

;Funciones para hallar la posicion exacta de un articulo en base a su nombre, obteniendo su fila y columna
  (defn encontrar-fila [liste nombre]
    (cond (empty? liste) false
          (= (first (rest (first liste))) nombre)  (first (first liste))
          :else (encontrar-fila (rest liste) nombre)))

  (defn encontrar-elemen [liste nombre]
    (cond (empty? liste) "elemento no encontrado"
          (= (encontrar-fila (first (rest (first liste))) nombre) false) (encontrar-elemen (rest liste) nombre)
          :else (list  (first (first liste)) (encontrar-fila (first (rest (first liste))) nombre))))

  (defn encontrar-elemento [carrusel nombre]
    (encontrar-elemen (last (leer-archivo carrusel)) nombre))

;Funcion para realizar el trayecto del carrusel a traves de un metodo efectivo para el camino mas rapido
;Despliega los movimientos que hace y va haciendo las actualizaciones en el archivo
  (defn trayecto [carrusel inicio final]
    (cond (= (first inicio) (first final)) (cond (= (first (rest inicio)) (first (rest final))) (println "Llegaste!")
                                                 (> (first (rest inicio)) (first (rest final)))  (do (izquierdon carrusel) (println "izquierda") (trayecto carrusel (list (first inicio) (- (first (rest inicio)) 1)) final))
                                                 :else (do  (derechon carrusel) (println "derecha")  (trayecto carrusel (list (first inicio) (+ (first (rest inicio)) 1)) final)))
          (> (first inicio) (first final)) (do (println "Abajo") (abajon carrusel) (trayecto carrusel (cons (- (first inicio) 1) (rest inicio))  final))
          :else (do (println "Arriba") (arriban carrusel) (trayecto carrusel (cons (+ (first inicio) 1) (rest inicio))  final))))


;Regresa el valor del nombre del articulo en que nos encontramos
  (defn nameactual [columna lista]
    (cond (= columna (first (first lista))) (first (rest (first lista)))
          :else (nameactual columna (rest lista))))

  (defn namer [carrusel]
    (nameactual (first (leer-archivo carrusel)) (last (first (last (leer-archivo carrusel))))))


;Regresa la posicion en la que nos encontramos
  (defn actual [carrusel]
    (list (first (first (last (leer-archivo carrusel)))) (first (leer-archivo carrusel))))



;Funcion para mandar traer a trayecto por default
  (defn trayectofn [carrusel final]
   
    (trayecto carrusel (actual carrusel) final))

;Funcion que busca en la fila el objeto y si es el nombre igual, suma la cantidad, de igual forma
;actualiza todo el carrusel
  (defn sumar [liste cantidad nombre]
    (map #(if (= (first (rest %)) nombre) (list (first %) (first (rest %)) (+ (first (rest (rest %))) cantidad) (last %)) %) liste))

  (defn agrega-producton [lista cantidad nombre]
    (cond (empty? lista) nil
          :else (cons (list (first (first lista)) (sumar (first (rest (first lista))) cantidad nombre)) (agrega-producton (rest lista) cantidad nombre))))

;Regresa la suma de todos los elementos
  (defn valor-elements [lista]
    (if (empty? lista) 0
        (+ (* (first (rest (rest (first lista))))  (last (first lista))) (valor-elements (rest lista)))))
  (defn valorin [lista]
    (if (empty? lista) 0
        (+ (valor-elements (first (rest (first lista)))) (valorin (rest lista)))))

  (defn valor-total [carrusel]
    (valorin (last (leer-archivo carrusel))))

;Funcion para mandar traer a agregar producto y escribir en el archivo de texto
  (defn agrega-producto-def [carrusel nombre cantidad]
    (spit carrusel
          (list (first (leer-archivo carrusel)) (agrega-producton (last (leer-archivo carrusel)) cantidad nombre))))
;Funcion para restar los productos y que actualiza el carrusel
  (defn restar [liste cantidad nombre]
    (map #(if (= (first (rest %)) nombre) (list (first %) (first (rest %)) (if (< (first (rest (rest %))) cantidad) 0 (- (first (rest (rest %))) cantidad)) (last %)) %) liste))

  (defn quita [lista cantidad nombre]
    (cond (empty? lista) nil
          :else (cons (list (first (first lista)) (restar (first (rest (first lista))) cantidad nombre)) (quita (rest lista) cantidad nombre))))
  (defn quitalo [carrusel nombre cantidad ]
    (spit carrusel
          (list (first (leer-archivo carrusel)) (quita (last (leer-archivo carrusel)) cantidad nombre))))

;Funciones para obtener los articulos con menor cantidad
  (defn menor [liste]
    (cond (empty? liste) nil
          (< (first (rest (rest (first liste)))) 10) (list* (first liste) (menor (rest liste)))
          :else (menor (rest liste))))

  (defn minvalue [liste]
    (cond (empty? liste) nil
          :else (concat (menor (last (first liste))) (minvalue (rest liste)))))

  (defn menores [carrusel]
    (minvalue (last (leer-archivo carrusel))))


;Funciones para en caso de agregar producto que llamara el usuario, de que si se coloca 1 o 2 argumentos
  (defn agregar-producto [& args]
    (cond (= (count args) 2) (do (agrega-producto-def (first args) (namer (first args)) (second args))
                                 (info-producto (first args)))
          (= (count args) 3) (do (trayectofn (first args) (encontrar-elemento (first args) (second args)))
                                 (agrega-producto-def (first args) (second args) (nth args 2))
                                 (info-producto (first args)))
          :else (println "Error: Se esperaba uno o dos argumentos")))

;Funciones para en caso de retirar producto que llamara el usuario, de que si se coloca 1 o 2 argumentos

  (defn retirar-producto [& args]
    (cond (= (count args) 2) (do (quitalo (first args) (namer (first args)) (second args))
                                 (info-producto (first args)))
          (= (count args) 3) (do ()
                                 (trayectofn (first args) (encontrar-elemento (first args) (second args)))

                                 (quitalo (first args) (second args) (nth args 2))
                                 (info-producto (first args)))
          :else (println "Error: Se esperaba uno o dos argumentos")))







;;Generacion de entrada final que engloba los articulos con menor cantidad y el valor total del carrusel
  (defn final [carrusel]
    (println "Articulos menores: " (menores carrusel))
    (println "Valor total del carrusel" (valor-total carrusel)))

;;Elegir el 10% de los datos
(defn topi [datos]
  (let [tot (count datos)
        orde (sort-by second datos)
        por (take-last (/ tot 10) orde)]
    por))

(require '[clojure.java.io :as io])

  
  ;;Ejecucion principal
(defn ejecutar-instrucciones [elemento]
  (with-open [rdr (io/reader (first (rest elemento)))]
    (let [lines (line-seq rdr)]
      (doseq [line lines]
        (println "INSTRUCCION: " line)
        (let [tokens (re-seq #"\S+" line)]
          (cond (= (first tokens) "retirar-producto")
                (cond (= (count tokens) 3)
                  (let [[_ producto cantidad] tokens] 
                    (retirar-producto (first elemento) (symbol producto) (Integer/parseInt cantidad))
                    )
                  (= (count tokens) 2) (let [[_ cantidad] tokens]
                                         (retirar-producto (first elemento) (Integer/parseInt cantidad)))
                      :else (println "mal numero de elementos"))
                (= (first tokens) "derechon") (derechon (first elemento))
                (= (first tokens) "izquierdon") (izquierdon (first elemento))
                (= (first tokens) "abajon") (abajon (first elemento))
                (= (first tokens) "arriban") (arriban (first elemento))
                (= (first tokens) "agregar-producto")
                (cond (= (count tokens) 3)
                  (let [[_ producto cantidad] tokens] 
                    (agregar-producto (first elemento) (symbol producto) (Integer/parseInt cantidad)) )
                       (= (count tokens) 2) (let [[_ cantidad] tokens]
                                              (agregar-producto (first elemento) (Integer/parseInt cantidad)))
                  :else (println "Instrucción agregar-producto mal formada"))
                (= (first tokens) "final") (final (first elemento))))))))

;Generacion de listas para pruebas
(defn generar-lista [x]
  (map (fn [i] (list (str "carrusel" i ".txt") (str "Instrucciones" i ".txt")))
       (range 1 (inc x))))


;Generacion de listas unicamente de carruseles
(defn generar-lista-carrus [x]
  (flatten (map (fn [i] (list (str "carrusel" i ".txt")))
                (range 1 (inc x)))))

(def values (generar-lista-carrus 40))


(def carruseles (generar-lista 40))


;Exportacion de salida a txt
(with-out-str
  (binding [*out* (clojure.java.io/writer "Ejecucion.txt")]
    (do (dorun (pmap (fn [x] (ejecutar-instrucciones x)) carruseles))
        (println) 
        (println)

        (println "Lista de carruseles con articulos por resurtir")

        (println  (apply list (map (fn [x]
                                     (if (empty? (menores x))
                                       '()
                                       (conj (menores x) (list (str "carrusel:" x "hace falta de")))))
                                   values))) 
        (println)
        (println)

        (println "Suma de articulos de todos los carruseles" (apply + (map valor-total values))) (println)
        (println)

        (println "Top 10% de carruseles: " (topi (apply list (map (fn [x] (list x (valor-total x))) values))))
        
        )))


 

