import pandas as pd

def data_precoessing(dataPath, outputPath):
    df = pd.read_csv(dataPath)

    ##非数值型特征处理
    x = pd.factorize(df['Geography'])
    y = pd.factorize(df['Gender'])
    df['Geography'] = x[0]
    df['Gender'] = y[0]
    ##数据离散化处理
    CreditScore = [];
    Age = [];
    EstimatedSalary = [];

    for i in range(len(df)):
        if df["CreditScore"][i] < 584:
            CreditScore.append(0)
        elif df["CreditScore"][i] < 718:
            CreditScore.append(1)
        else:
            CreditScore.append(2)
    df["CreditScore"] = CreditScore

    for i in range(len(df)):
        if df["Age"][i] <= 20:
            Age.append(0)
        elif df["Age"][i] <= 40:
            Age.append(1)
        else:
            Age.append(2)
    df["Age"] = Age

    for i in range(len(df)):
        if df["EstimatedSalary"][i] < 51002:
            EstimatedSalary.append(0)
        elif df["EstimatedSalary"][i] < 149388:
            EstimatedSalary.append(1)
        else:
            EstimatedSalary.append(2)
    df["EstimatedSalary"] = EstimatedSalary
    df.to_csv(outputPath)


data_precoessing("./train.csv", "./train_precoessing.csv")
data_precoessing("./test.csv", "./test_precoessing.csv")