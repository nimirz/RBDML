import pandas as pd
from sklearn.model_selection import train_test_split
from xgboost import XGBClassifier
from sklearn.metrics import accuracy_score, classification_report

# Load your merged dataset (climate and disease data)
# df = pd.read_csv("your_dataset.csv")

# Assume 'climate_feature1', 'climate_feature2', ... are your climate features
# 'disease_cases' is the target variable (1 for high, 0 for low)

# Data Preprocessing
# Handle missing values, outliers, and feature engineering as needed

# Feature Selection
features = ['climate_feature1', 'climate_feature2', ...]
X = df[features]
y = df['disease_cases']

# Data Splitting
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Model Training
model = XGBClassifier()
model.fit(X_train, y_train)

# Model Evaluation
y_pred = model.predict(X_test)
accuracy = accuracy_score(y_test, y_pred)
classification_report_result = classification_report(y_test, y_pred)

print(f"Accuracy: {accuracy}")
print("Classification Report:\n", classification_report_result)