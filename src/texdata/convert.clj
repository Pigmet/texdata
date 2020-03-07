(ns texdata.convert
  (:import 
   (java.io IOException File)
   (java.awt.image BufferedImage)
   (javax.imageio ImageIO)
   (org.apache.pdfbox.rendering PDFRenderer ImageType)
   (org.apache.pdfbox.pdmodel PDDocument)))

(def sample-file "test/texdata/examples/out/test.pdf")

(def out-path "test/texdata/examples/out/test.png")

(defn- new-doc [path]
(PDDocument/load (new File path)))

(defn- new-renderer [doc] (new PDFRenderer doc))

(defn- new-buffered-image [r page]
(.renderImageWithDPI r page 300 ImageType/RGB))

(defn- write-buffered-image [image path]
  (ImageIO/write
   image
   "png"
   (new File path)))

(comment

  (def result-bim
    (-> sample-file new-doc new-renderer (new-buffered-image 0)))

  (write-buffered-image result-bim out-path)


  )
