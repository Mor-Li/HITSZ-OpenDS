import org.apache.spark.ml.linalg.Vector
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.SparkSession
object DataTypes {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .master("local[*]")
      .appName("DataTypes")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    // 创建dense密集向量
    val dv: Vector = Vectors.dense(1,-4,3)
    println(dv) //[1.0,2.0,3.0]

    println(Vectors.norm(dv,1))
    println(Vectors.norm(dv,2))

    // 创建sparse稀疏向量
    val sv1: Vector = Vectors.sparse(3, Array(0,2), Array(1.0,3.0))
    println(sv1) //(3,[0,2],[1.0,3.0])
    val sv2: Vector = Vectors.sparse(3, Seq((0, 1.0),(2,3.0)))
    println(sv2) //(3,[0,2],[1.0,3.0])


    spark.stop()
  }

}
