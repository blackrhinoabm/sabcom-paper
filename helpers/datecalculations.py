from datetime import datetime

def days_between(d1, d2):
    d1 = datetime.strptime(d1, "%Y-%m-%d")
    d2 = datetime.strptime(d2, "%Y-%m-%d")
    return abs((d2 - d1).days)
	
def generate_new_series(old_series, all_dates):
    new_series = []
    for d in all_dates:
        if d in old_series.index:
            new_series.append(old_series.loc[d])
        else:
            new_series.append(0)
    return new_series