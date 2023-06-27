package com.tipdm
import org.apache.spark.mllib.recommendation.{MatrixFactorizationModel, ALS, Rating}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkContext, SparkConf}

/**
 * 为创建ALS推荐模型寻找最优参数组，需要输入以下参数
 * trainDataPath: 训练数据的路径
 * validateDataPath: 训练数据的路径
 * listRank: 可选的用户项目子矩阵的阶值列表
 * listIteration: 可选的循环次数值列表
 * listLambda: 可选的防止过拟合参数值列表
 * paraOutputPath：最优参数组存储目录
 * splitter:输入原始数据分隔符
 */
object ALSModelOptimize {
  val appName = "Optimize ALS Model Parameters"
  val conf = new SparkConf().setAppName(appName)
  val sc = new SparkContext(conf)
  sc.setLogLevel("WARN")

  def main(args: Array[String]) = {
    if (args.length != 7) {
      System.err.println("Usage:ALSModelOptimize requires: 7 input fields ")
    }
    // 匹配输入参数
    val trainDataPath = args(0)
    val validateDataPath = args(1)
    val listRank = args(2).split(",").map(_.toInt)
    val listIteration = args(3).split(",").map(_.toInt)
    val listLambda = args(4).split(",").map(_.toDouble)
    val paraOutputPath = args(5)
    val splitter = args(6)

    // 定义计算均方根误差的函数：computeRMSE
    def computeRMSE(model:MatrixFactorizationModel, data:RDD[Rating]): Double = {
      val usersProducts = data.map(x=>(x.user,x.product))
      val ratingsAndPredictions = data.map{case Rating(user,product,rating)=>((user,product),rating)
      }.join(model.predict(usersProducts).map{case Rating(user,product,rating)=>((user,product),rating)}).values ;
      math.sqrt(ratingsAndPredictions.map(x=>(x._1-x._2)*(x._1-x._2)).mean())}

    // 加载训练集数据
    val trainData = sc.textFile(trainDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter);
      (fields(0).toInt,fields(1).toInt,fields(2).toDouble)}
    val trainDataRating= trainData.map(x=>Rating(x._1,x._2,x._3))
    // 加载验证集数据
    val validateData = sc.textFile(validateDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter);
      (fields(0).toInt,fields(1).toInt,fields(2).toDouble)}
    val validateDataRating= validateData.map(x=>Rating(x._1,x._2,x._3))
   // 初始化最优参数，取极端值
    var bestRMSE = Double.MaxValue
    var bestRank = -10
    var bestIteration = -10
    var bestLambda = -1.0
    // 参数寻优
    for( rank<- listRank;lambda<-listLambda;iter<-listIteration) {
      val model = ALS.train(trainDataRating,rank,iter,lambda);
      val validationRMSE = computeRMSE(model,validateDataRating);
      if(validationRMSE<bestRMSE){
        bestRMSE=validationRMSE;
        bestRank=rank;
        bestLambda=lambda;
        bestIteration=iter}
        }
    // 输出最优参数组
    println("BestRank:Iteration:BestLambda => BestRMSE")
    println(bestRank + ": " + bestIteration + ": " + bestLambda + " => " + bestRMSE)

    val result = Array(bestRank + "," + bestIteration + "," + bestLambda)
    sc.parallelize(result).repartition(1).saveAsTextFile(paraOutputPath)
    sc.stop()
  }
}
