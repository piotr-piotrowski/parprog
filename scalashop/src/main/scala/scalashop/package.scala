
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
     var redC = 0
     var greenC = 0
     var blueC = 0
     var alphaC = 0
     var startX = if (x - radius <= 0) 0 else x - radius
     val endX = if (x + radius > src.width - 1) src.width - 1 else x + radius
     var startY = if (y - radius <= 0) 0 else y - radius
     val endY = if (y + radius > src.height - 1) src.height - 1 else y + radius
     val size = (endX - startX + 1) * (endY - startY + 1)
     while (startY <= endY) {
	     while (startX <= endX) {
		     val pixel = src.apply(startX, startY)
		     redC = redC + red(pixel)
		     greenC = greenC + green(pixel)
		     blueC = blueC + blue(pixel)
		     alphaC = alphaC + alpha(pixel)
		     startX = startX + 1
	     }
       startX = if (x - radius < 0) 0 else x - radius
	     startY = startY + 1
     }
     rgba(redC/size, greenC/size, blueC/size, alphaC/size)
  }

}
