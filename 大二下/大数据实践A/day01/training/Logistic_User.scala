package training
import org.apache.spark.{ SparkConf, SparkContext }
import org.apache.spark.SparkContext
import  org.apache.spark.mllib.classification.{LogisticRegressionWithLBFGS,LogisticRegressionModel}
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
object Logistic_User {
  def main(args:Array[String]): Unit ={

    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("logistic")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")
   /* if(args.length != 6) {
      println("output args")
      System.exit(1)
    }*/

    //设置需要的参数
    val inpath = "inputdata/data3" //输入数据路径
    val model_logistic = "inputdata/model" //模型存储位置
    val f1Score_path = "inputdata/data1" //F 值输出路径
    val threshold = 0.5 //阈值
    val splitter = "," //数据分隔符
    val bili = 0.8 //训练数据占比

    //所读取数据转化成模型数据并且划分为训练集和测试集
    //稠密向量 Vectors.dense
    //LabeledPoint 数据类型
    val data = sc.textFile(inpath)
        .map{x=> val lines = x.split(splitter);
          LabeledPoint(lines(0).toDouble,Vectors.dense(lines.slice(1,lines.length).map(_.toDouble)))};

    //分割 training and test
    val splits = data.randomSplit(Array(bili, 1-bili), seed = 11L)
    val training = splits(0).cache()
    val test = splits(1)
    //训练模型
    val model = new LogisticRegressionWithLBFGS()
      .setNumClasses(2) //设置分类数量
      .run(training)
      .setThreshold(threshold)  //设置阈值

   //预测
    val predictionAndLabels = test.map{ case LabeledPoint(label, features) =>
      //预测
      val prediction = model.predict(features)
      (prediction, label)  //预测类别和真实类别组成的二元组
    }
    predictionAndLabels.take(10).foreach(println)
    //
    val metrics = new BinaryClassificationMetrics(predictionAndLabels)

    //准确率
    val precision = metrics.precisionByThreshold
    precision.collect().foreach{
      case (t,p)=>println(s"Threshold: $t,Precision: $p")
    }
    // Recall by threshold
    //召回率
    val recall = metrics.recallByThreshold
    recall.collect().foreach{
      case (t,r)=>println(s"Threshold: $t,Recall: $r")
    }
    // F-measure
    val f1Score = metrics.fMeasureByThreshold
    f1Score.collect().foreach{
      case (t,f)=>println(s"Threshold: $t,F1Score: $f")
    }

    val roc = metrics.roc
    val auRoc = metrics.areaUnderROC
    println("Area under ROC: "+ auRoc)

    f1Score.repartition(1).saveAsTextFile(f1Score_path)
    //模型存储
    model.save(sc,model_logistic)


  }
}