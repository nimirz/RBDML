'''
Classification Models. 

'''
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix

class RandomForestClassifier:
    def __init__(self, classifier=None):
        if classifier is None:
            self.classifier = RandomForestClassifier(n_estimators=100, random_state=42)
        else:
            self.classifier = classifier
    
    def train(self, X_train, y_train):
        self.classifier.fit(X_train, y_train)
    
    def evaluate(self, X, y_true):
        y_pred = self.classifier.predict(X)
        accuracy = accuracy_score(y_true, y_pred)
        precision = precision_score(y_true, y_pred)
        recall = recall_score(y_true, y_pred)
        f1 = f1_score(y_true, y_pred)
        confusion = confusion_matrix(y_true, y_pred)
        return accuracy, precision, recall, f1, confusion
    
    def predict(self, X):
        return self.classifier.predict(X)
