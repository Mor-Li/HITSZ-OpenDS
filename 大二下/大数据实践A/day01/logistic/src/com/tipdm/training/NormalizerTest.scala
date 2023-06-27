package com.tipdm.training
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.ml.linalg.{Vector, Vectors}
import org.apache.spark.sql.hive.HiveContext
import org.apache.spark.ml.feature.Normalizer
import org.apache.spark.ml.feature.StandardScaler
import org.apache.spark.ml.feature.MinMaxScaler

object NormalizerTest {

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("NormalizerTest")
    val sc = new SparkContext(conf)
    val sqlContext = new HiveContext(sc)
    sc.setLogLevel("WARN")

    val dataFrame = sqlContext.createDataFrame(Seq(
      (0,Vectors.dense(1.0, 0.5, -1.0)),
      (1,Vectors.dense(2.0, 1.0, 1.0)),
      (2,Vectors.dense(4.0, 10.0, 2.0))
    )).toDF("id", "features")
    dataFrame.show()
    // Normalizer 规范化  setP(1.0)表示L1范数
    val normalizer = new Normalizer()
      .setInputCol("features")
      .setOutputCol("normFeatures").setP(1.0)
    val normData_trans = normalizer.transform(dataFrame)
    normData_trans.show()
//    //StandardScaler ---去均值和方差归一化
    val scaler1 = new StandardScaler().setInputCol("features")
      .setOutputCol("StandardScaler")
      .setWithStd(true).setWithMean(false)
    val scalerModel1 = scaler1.fit(dataFrame)
    val scaledData1 = scalerModel1.transform(dataFrame)
    scaledData1.show(false)
//    // MinMaxScaler 示例  最大-最小规范化
    val scaler = new MinMaxScaler()
      .setInputCol("features")
      .setOutputCol("MinMaxScaler")
    val scalerModel = scaler.fit(dataFrame)
    val scaledData = scalerModel.transform(dataFrame)
    scaledData.show(false)
  }

}
