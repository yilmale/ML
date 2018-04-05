import java.awt.{Color, Graphics, Graphics2D}
import javax.swing.{JFrame, JPanel}

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.data.general.DefaultPieDataset
import org.jfree.data.general.PieDataset
import org.jfree.ui.ApplicationFrame
import org.jfree.ui.RefineryUtilities

object Vis {

  class X {
    var p : String = _

    def setP(x: String): Unit = {
      p = x
    }

    def getP: String = {
      p
    }
  }

  class PieChart_AWT(s : String) extends ApplicationFrame(s) {
    setContentPane(createDemoPanel())


    private def createDataset = {
      val dataset = new DefaultPieDataset
      dataset.setValue("IPhone 5s", 20.0)
      dataset.setValue("SamSung Grand", 20.0)
      dataset.setValue("MotoG", 40.0)
      dataset.setValue("Nokia Lumia", 10.0)
      dataset
    }


    private def createChart(dataset: PieDataset) = {
      val chart = ChartFactory.createPieChart(s, // chart title
        dataset, // data
        true, // include legend
        true, false)
      chart
    }

    def createDemoPanel() : JPanel = {
      val chart : JFreeChart = createChart(createDataset)
      new ChartPanel(chart)
    }

  }


  def apply(): Unit = {
    val myX : X = new X()
    myX.setP("test")
    println("new value is " + myX.getP)


    val demo  : PieChart_AWT = new PieChart_AWT("Test")
    demo.setSize( 560 , 367 )
    RefineryUtilities.centerFrameOnScreen(demo)
    demo.setVisible( true )

  }

  class GridVis extends JPanel {

    override def paintComponent(g: Graphics) {
      super.paintComponent(g)
      var g2d = g.asInstanceOf[Graphics2D]

      g2d.setColor(new Color(212, 212, 212))
      for (y <- 0 to 25)
        for (x <- 0 to 25)
          g2d.drawRect(10+20*x, 15+20*y, 20, 20)


      g2d.setColor(new Color(31, 21, 1))

      val x = 5
      val y = 5
      g2d.fillRect(10+20*x, 15+20*y, 20, 20)

    }
  }

  object GridVis {
    def createGridVis(x : Int, y : Int) :GridVis = {
      val rects = new GridVis()
      val frame = new JFrame("Grid")
      //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.add(rects)
      frame.setSize(360, 300)
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
      rects
    }
  }

}