import org.apache.spark.ml.feature.LabeledPoint
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.SparkSession

object LabelType {
  def main(args: Array[String]): Unit = {

    val spark = SparkSession.builder()
      .master("local[*]")
      .appName("LabelType")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    // Create a labeled point with a positive label and a dense feature vector.
    val pos = LabeledPoint(1.0, Vectors.dense(1.0, 0.0, 3.0))
    println(pos)
    // Create a labeled point with a negative label and a sparse feature vector.
    val neg = LabeledPoint(0.0, Vectors.sparse(3, Array(0, 2), Array(1, 3)))
    println(neg)

  }
}
