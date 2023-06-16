import pandas as pd
# from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import KFold
from sklearn.metrics import precision_recall_fscore_support
import numpy as np
import matplotlib.pyplot as plt
from sklearn.tree import plot_tree
from sklearn.tree import ExtraTreeClassifier
from sklearn.ensemble import RandomForestClassifier

def kfold_test(X,y,clf):
    # 使用 KFold 进行k折交叉验证
    kf = KFold(n_splits=3, shuffle=True, random_state=42)
    precision_scores = []
    recall_scores = []
    f1_scores = []
    for train_index, val_index in kf.split(X):
        X_train, X_val = X[train_index], X[val_index]
        y_train, y_val = y[train_index], y[val_index]

        clf.fit(X_train, y_train)
        y_pred = clf.predict(X_val)
        precision, recall, f1, _ = precision_recall_fscore_support(y_val, y_pred, average='weighted')
        precision_scores.append(precision)
        recall_scores.append(recall)
        f1_scores.append(f1)
    # 计算平均评价指标
    mean_precision = np.mean(precision_scores)
    mean_recall = np.mean(recall_scores)
    mean_f1 = np.mean(f1_scores)
    print(f"平均模型性能：\nPrecision: {mean_precision}\nRecall: {mean_recall}\nF1: {mean_f1}")
    # print(f"平均模型性能：\nPrecision: {precision_scores}\nRecall: {recall_scores}\nF1: {f1_scores}")
    
if __name__=='__main__':
    df = pd.read_csv('train.csv')

    X = df.iloc[:, 1:-1].values
    y = df.iloc[:, -1].values

    clf = RandomForestClassifier(random_state=42,
                             max_depth=15,
                             max_features= 4,
                             max_leaf_nodes=100, 
                             min_samples_split=10,
                             min_samples_leaf=10,
                             n_estimators=50,
                             bootstrap=True,
                             )
    '''
    clf = DecisionTreeClassifier(random_state=42,
                             criterion='gini',
                             splitter='best',
                             max_depth=7,
                             max_features= 13,#7,11,13,17
                             max_leaf_nodes=15, 
                             min_samples_split=25,
                             min_samples_leaf=10,
                             )
    '''

    kfold_test(X,y,clf)
    # 使用全部数据训练模型
    clf.fit(X,y)

    df_test = pd.read_csv('test_nolabel.csv')
    X_train= df_test.iloc[:, 1:].values
    y_pred2=clf.predict(X_train)
    # 先将y_pred添加到df中作为一列
    df_test["y"] = y_pred2
    # 然后选择要输出的列
    df_test.drop(df.columns[1:21], axis=1, inplace=True)
    # print(df_test.columns)
    # 最后将这些列输出到submit.csv文件中
    df_test.to_csv("submit.csv",index=False)