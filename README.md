link to data:
https://huggingface.co/datasets/scikit-learn/churn-prediction

questions for tommorow:
- when dealing with factors (PaymentMethod) with more levels should i create dummies?
- how to deal with dummies that are correlated (factor with 4 levels, and last is NA)
- if we reject H0 that the model is better with the rejected variable than without, should we continue removing next variables if we fail to reject their H0
  (after removing PaymentMethod and rejecting H0, can we than continue to remove OnlineSecurity
