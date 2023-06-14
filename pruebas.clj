(defn obtener-elemento-aleatorio [ruta-archivo]
  (let [contenido (slurp ruta-archivo)
        lista-texto (clojure.string/split-lines contenido)
        lista-quotes (mapv #(read-string %) lista-texto)
        quote-aleatorio (rand-nth lista-quotes)]
    quote-aleatorio))


(defn generar [filas columnas]
  (letfn [(generar-fila [n]
            (loop [m columnas
                   inner-result '()]
              (if (>= m 1)
                (recur (dec m)
                       (conj inner-result
                             (list m (obtener-elemento-aleatorio "articulos.txt") n (* n m))))
                inner-result)))]
    (loop [n filas
           result '()]
      (if (>= n 1)
        (recur (dec n)
               (conj result (list n (generar-fila n))))
        result))))


(defn generating-def [filas columnas]
  (list 1 (apply list (generar filas columnas))))

;(println (apply list (generar 22 5)))
;(println (generating-def 22 5))




(require '[clojure.string :as str])


(defn generar-carruseles [cantidad filas columnas]
  (doseq [i (range 6 (inc cantidad))]
    (let [nombre-archivo (str "carrusel" i ".txt")
          carrusel (generating-def filas columnas)]
      (spit nombre-archivo carrusel))))

(generar-carruseles 40 22 5)





;; Ejemplo de uso: generar 10 repeticiones de instrucciones
(defn generar-archivos [n]
  (doseq [x (range 5 (inc n))]
    (let [nombre-archivo (str "Instrucciones" x ".txt")
          contenido (str "retirar-producto 8\n"
                         "derechon\n"
                         "izquierdon\n"
                         "abajon\n"
                         "agregar-producto 11\n"
                         "arriban\n"
                         "final\n")]
      
        (spit nombre-archivo contenido))))

(generar-archivos 40)
;(println (generar-instrucciones (generating-def 22 5)))