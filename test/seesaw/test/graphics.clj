;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.graphics
  (:use seesaw.graphics
        [seesaw.color :only [to-color]])
  (:use clojure.test
        )
  (:import [java.awt RenderingHints]
           [java.awt.image BufferedImage]))

(deftest anti-alias-test
  (testing "turns on anti-aliasing on a graphics object"
    (let [bi (buffered-image 100 100)
          g2d (.getGraphics bi)]
      (anti-alias g2d)
      (is (= RenderingHints/VALUE_ANTIALIAS_ON (.getRenderingHint g2d RenderingHints/KEY_ANTIALIASING))))))

(deftest to-paint-test
  (testing "returns its input if it's a java.awt.Paint"
    (= java.awt.Color/BLACK (to-paint java.awt.Color/BLACK)))
  (testing "falls back to to-color otherwise"
    (= (to-color :black) (to-paint :black))))

(deftest line-test
  (testing "creates a line shape with given end points"
    (let [l (line 1 2 3 4)]
      (is (= java.awt.geom.Line2D$Double (class l)))
      (is (= [1.0 2.0 3.0 4.0] [(.x1 l) (.y1 l) (.x2 l) (.y2 l)])))))

(deftest rect-test
  (testing "creates a rectangle shape with give corner, width and height"
    (let [r (rect 1 2 3 4)]
      (is (= java.awt.geom.Rectangle2D$Double (class r)))
      (is (= [1.0 2.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))))
  (testing "creates a rectangle shape with give corner, adjusting for negative width and height"
    (let [r (rect 10 20 -3 -4)]
      (is (= java.awt.geom.Rectangle2D$Double (class r)))
      (is (= [7.0 16.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))))
  (testing "creates a square with give corner, and side length"
    (let [r (rect 1 2 3)]
      (is (= java.awt.geom.Rectangle2D$Double (class r)))
      (is (= [1.0 2.0 3.0 3.0] [(.x r) (.y r) (.width r) (.height r)])))))

(deftest rounded-rect-test
  (testing "creates a rounded rectangle shape with give corner, width and height and radii"
    (let [r (rounded-rect 1 2 3 4 5 6)]
      (is (= java.awt.geom.RoundRectangle2D$Double (class r)))
      (is (= [1.0 2.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))
      (is (= [5.0 6.0] [(.arcwidth r) (.archeight r)]))))
  (testing "creates a rounded rectangle shape with give corner, negative width and height and radii"
    (let [r (rounded-rect 10 20 -3 -4 5 6)]
      (is (= java.awt.geom.RoundRectangle2D$Double (class r)))
      (is (= [7.0 16.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))
      (is (= [5.0 6.0] [(.arcwidth r) (.archeight r)]))))
  (testing "creates a rounded rectangle shape with give corner, width and height and radius"
    (let [r (rounded-rect 1 2 3 4 5)]
      (is (= java.awt.geom.RoundRectangle2D$Double (class r)))
      (is (= [1.0 2.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))
      (is (= [5.0 5.0] [(.arcwidth r) (.archeight r)])))))

(deftest ellipse-test
  (testing "creates an elliptical shape with give corner, width and height"
    (let [r (ellipse 1 2 3 4)]
      (is (= java.awt.geom.Ellipse2D$Double (class r)))
      (is (= [1.0 2.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))))
  (testing "creates an elliptical shape with give corner, negative width and height"
    (let [r (ellipse 11 12 -3 -4)]
      (is (= java.awt.geom.Ellipse2D$Double (class r)))
      (is (= [8.0 8.0 3.0 4.0] [(.x r) (.y r) (.width r) (.height r)]))))
  (testing "creates a square with give corner, and side length"
    (let [r (ellipse 1 2 3)]
      (is (= java.awt.geom.Ellipse2D$Double (class r)))
      (is (= [1.0 2.0 3.0 3.0] [(.x r) (.y r) (.width r) (.height r)])))))

(deftest circle-test
  (testing "creates a circle with center and radius"
    (let [r (circle 4 5 6)]
      (is (= java.awt.geom.Ellipse2D$Double (class r)))
      (is (= [-2.0 -1.0 12.0 12.0] [(.x r) (.y r) (.width r) (.height r)])))))

(deftest arc-test
  (testing "creates an arc shape with corner, width, height and angle"
    (let [s (arc 1 2 3 4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/OPEN (.getArcType s)))
      (is (= [1.0 2.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)]))))
  (testing "creates an arc shape with corner, negative width, negative height and angle"
    (let [s (arc 12 22 -3 -4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/OPEN (.getArcType s)))
      (is (= [9.0 18.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)])))))

(deftest chord-test
  (testing "creates an chord shape with corner, width, height and angle"
    (let [s (chord 1 2 3 4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/CHORD (.getArcType s)))
      (is (= [1.0 2.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)]))))
  (testing "creates an chord shape with corner, negative width, negative height and angle"
    (let [s (chord 10 21 -3 -4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/CHORD (.getArcType s)))
      (is (= [7.0 17.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)])))))

(deftest pie-test
  (testing "creates an pie shape with corner, width, height and angle"
    (let [s (pie 1 2 3 4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/PIE (.getArcType s)))
      (is (= [1.0 2.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)]))))
  (testing "creates an pie shape with corner, negative width, negative height and angle"
    (let [s (pie 11 20 -3 -4 0 360)]
      (is (= java.awt.geom.Arc2D$Double (class s)))
      (is (= java.awt.geom.Arc2D/PIE (.getArcType s)))
      (is (= [8.0 16.0 3.0 4.0 0.0 360.0]
                 [(.x s) (.y s) (.width s) (.height s) (.start s) (.extent s)])))))

(deftest string-shape-test
  (testing "creates a string shape"
    (string-shape 1 2 "HI")))

(deftest stroke-test
  (testing "creates a default stroke of width 1 with no args"
    (let [s (stroke)]
      (is (= java.awt.BasicStroke (class s)))
      (is (= 1.0 (.getLineWidth s)))))
  (testing "creates a stroke with the given properties"
    (let [s (stroke :width 10, :cap :butt, :join :bevel, :miter-limit 15.0,
                    :dashes [10.0 5.0],
                    :dash-phase 2.0)]
      (is (= java.awt.BasicStroke (class s)))
      (is (= 10. (.getLineWidth s)))
      (is (= java.awt.BasicStroke/CAP_BUTT (.getEndCap s)))
      (is (= 15.0 (.getMiterLimit s)))
      (is (= [10.0 5.0] (seq (.getDashArray s))))
      (is (= 2.0 (.getDashPhase s)))
      (is (= java.awt.BasicStroke/JOIN_BEVEL (.getLineJoin s))))))

(deftest to-stroke-test
  (testing "throws IllegalArgumentException if it doesn't know what to do"
    (try
      (do (to-stroke #"what?") false)
      (catch IllegalArgumentException e true)))
  (testing "returns nil for nil input"
    (nil? (to-stroke nil)))
  (testing "returns a stroke of a given width if input is a number"
    (= 10.0 (.getLineWidth (to-stroke 10))))
  (testing "returns input if it's a stroke"
    (let [s (stroke)]
      (is (= s (to-stroke s))))))

; make a stub shape to grab args...
(defrecord TestShape [received-args]
  Draw
  (draw* 
    [shape g2d style] 
    ; Note that "this" is used instead of shape. Otherwise, on failure, lazytest
    ; tries to print a cyclical structure when there's a failure.
    (swap! received-args conj [g2d "this" style])))

(deftest draw-test
  (testing "should call Draw/draw* with graphics, shape and style"
    (let [args (atom [])
          ts (TestShape. args)
          result (draw "graphics" ts "style")
          final-args @args]
      (is (= "graphics" result))
      (is (= [["graphics" "this" "style"]] final-args))))

  (testing "should call Draw/draw* with graphics, and multiple shapes and styles"
    (let [args (atom [])
          ts (TestShape. args)
          result (draw "graphics" ts "style" ts "style2")
          final-args @args]
      (is (= "graphics" result))
      (is (= [["graphics" "this" "style"]["graphics" "this" "style2"]] final-args)))))
  
(deftest style-test
  (testing "creates a new style object"
    (let [strk (stroke :width 5)
          s (style :foreground :black :background :white :stroke strk :font :monospace)]
      (is (= java.awt.Color/BLACK (:foreground s)))
      (is (= java.awt.Color/WHITE (:background s)))
      (is (= strk (:stroke s)))
      (is (not (nil? (:font s)))))))

(deftest update-style-test
  (testing "constructs a new style with new property values"
    (let [strk (stroke :width 5)
          s (update-style (style :foreground :black :stroke strk) :foreground :white :background :black)]
      (is (instance? seesaw.graphics.Style s))
      (is (= java.awt.Color/WHITE (:foreground s)))
      (is (= java.awt.Color/BLACK (:background s)))
      (is (= strk (:stroke s)))))
  (testing "constructs a new style and can clear property values"
    (let [s (update-style (style :foreground :black) :foreground nil)]
      (is (instance? seesaw.graphics.Style s))
      (is (nil? (:foreground s))))))

(deftest linear-gradient-test
  (testing "creates a default linear gradient"
    (let [g (linear-gradient)]
      (is (= (java.awt.geom.Point2D$Float. 0.0 0.0)
                 (.getStartPoint g)))
      (is (= (java.awt.geom.Point2D$Float. 1.0 0.0)
                 (.getEndPoint g)))
      (is (= [(float 0.0) (float 1.0)]
                 (vec (.getFractions g))))
      (is (= [java.awt.Color/WHITE java.awt.Color/BLACK]
                 (vec (.getColors g))))
      (is (= java.awt.MultipleGradientPaint$CycleMethod/NO_CYCLE
                 (.getCycleMethod g)))))
  (testing "creates a linear gradient"
    (let [g (linear-gradient 
              :start [1 2] 
              :end [3.5 4.6]
              :fractions [0.0 0.8 1.0]
              :colors [:black :blue java.awt.Color/ORANGE]
              :cycle :repeat)]
      (is (= (java.awt.geom.Point2D$Float. 1.0 2.0)
                 (.getStartPoint g)))
      (is (= (java.awt.geom.Point2D$Float. 3.5 4.6)
                 (.getEndPoint g)))
      (is (= [(float 0.0) (float 0.8) (float 1.0)]
                 (vec (.getFractions g))))
      (is (= [java.awt.Color/BLACK java.awt.Color/BLUE java.awt.Color/ORANGE]
                 (vec (.getColors g))))
      (is (= java.awt.MultipleGradientPaint$CycleMethod/REPEAT
                 (.getCycleMethod g))))))

(deftest radial-gradient-test
  (testing "creates a default radial gradient"
    (let [g (radial-gradient)]
      (is (= (java.awt.geom.Point2D$Float. 0.0 0.0)
                 (.getCenterPoint g)))
      (is (= (java.awt.geom.Point2D$Float. 0.0 0.0)
                 (.getFocusPoint g)))
      (is (= (float 1.0) (.getRadius g)))
      (is (= [(float 0.0) (float 1.0)]
                 (vec (.getFractions g))))
      (is (= [java.awt.Color/WHITE java.awt.Color/BLACK]
                 (vec (.getColors g))))
      (is (= java.awt.MultipleGradientPaint$CycleMethod/NO_CYCLE
                 (.getCycleMethod g)))))
  (testing "creates a radial gradient"
    (let [g (radial-gradient 
              :center [1 2] 
              :focus [3.5 4.6]
              :fractions [0.0 0.8 1.0]
              :colors [:black :blue java.awt.Color/ORANGE]
              :cycle :reflect)]
      (is (= (java.awt.geom.Point2D$Float. 1.0 2.0)
                 (.getCenterPoint g)))
      (is (= (java.awt.geom.Point2D$Float. 3.5 4.6)
                 (.getFocusPoint g)))
      (is (= [(float 0.0) (float 0.8) (float 1.0)]
                 (vec (.getFractions g))))
      (is (= [java.awt.Color/BLACK java.awt.Color/BLUE java.awt.Color/ORANGE]
                 (vec (.getColors g))))
      (is (= java.awt.MultipleGradientPaint$CycleMethod/REFLECT
                 (.getCycleMethod g))))))

