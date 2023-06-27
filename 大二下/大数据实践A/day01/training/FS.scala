import org.apache.spark.ml.linalg.{Vector, Vectors}
import org.apache.spark.{SparkConf, SparkContext}

object FS {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("logistic")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val norm1Vec = Vectors.dense(1.0,-1.0,2.0)
    // 第一范数，就是绝对值相加
    println(Vectors.norm(norm1Vec,1)) // 4.0
    // 第二范数，就是平方和开根号
    println(Vectors.norm(norm1Vec,2)) // 2.449489742783178
    // 无限范数
    println(Vectors.norm(norm1Vec,100)) //2.0
    val sq1 = Vectors.dense(1.0, 2.0, 3.0)
    val sq2 = Vectors.dense(2.0, 4.0, 6.0)
    println(Vectors.sqdist(sq2, sq1))
    // (2-1)^2 + (4-2)^2 + (6-3)^2 = 14
    println("--------")
    val sv2: Vector = Vectors.sparse(3, Seq((0, 1.0),(2,3.0)))
    println(sv2) //(3,[0,2],[1.0,3.0])
    println(Vectors.norm(sv2,2))

    val sv1: Vector = Vectors.sparse(3, Array(0,2), Array(1.0,3.0))
    println(sv1)
    println(Vectors.norm(sv1,2))
    println("-----11---")
    println(Vectors.sqdist(sv2, sv1))
  }

}
