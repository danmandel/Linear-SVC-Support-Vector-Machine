import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm
from matplotlib import style
style.use("ggplot")

x=[1,5,1.5,8,1,9]
y=[2,8,1.8,8,0.6,11]


X = np.array(zip(x,y))

#targets
y = [0,1,0,1,0,1]

clf = svm.SVC (kernel = 'linear', C = 1.0)
clf.fit(X,y)

#print (clf.predict([0.58,0.76]))

w = clf.coef_[0]
a= -w[0] / w[1]

xx = np.linspace(0,12)
yy = a * xx - clf.intercept_[0] / w[1]

h0 = plt.plot(xx,yy,'k-', label="non-weighted divide")

plt.scatter(X[:,0],X[:, 1], c = y)
plt.legend()
plt.show()
