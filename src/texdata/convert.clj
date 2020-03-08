(ns texdata.convert
  (:import 
   (java.io File)
   (javax.imageio ImageIO)
   (org.apache.pdfbox.rendering PDFRenderer ImageType)
   (org.apache.pdfbox.pdmodel PDDocument)))

(defn- new-doc [path]
  (PDDocument/load (new File path)))

(defn- new-renderer [doc] (new PDFRenderer doc))

(defn- new-buffered-image [r page]
(.renderImageWithDPI r page 300 ImageType/RGB))

(defn- write-buffered-image [image path type-s]
  (ImageIO/write
   image
   type-s
   (new File path)))

(defn pdf->image [from-path out-path &{:keys [type] :or {type "png"}}]
  (let [doc (new-doc from-path)
        renderer (new-renderer doc)
        im (new-buffered-image renderer 0)]
    (write-buffered-image im out-path type)))

