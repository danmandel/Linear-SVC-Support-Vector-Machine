import matplotlib.pyplot as plt
from sklearn import datasets, svm


digits = datasets.load_digits()

#classifier
#gamma is the "leap size" used to arrive at the answer.
#gamma affects processing speed, accuracy
clf = svm.SVC(gamma=.001,C=100)

#loads all data into training set except for last 10 points
#fits data based on the answer in target
x,y = digits.data[:-10], digits.target[:-10]
clf.fit(x,y)

#predicts what the -1 aka last element is
print 'Prediction:', int(clf.predict(digits.data[-1]))
print 'Actual:', digits.target[-1]

#shows what the actual image is
plt.imshow(digits.images[-1], cmap=plt.cm.gray_r,interpolation="nearest")
plt.show()
