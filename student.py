run profile1

# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3   
 
# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 69.69

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)

# =============================================================================
# G2                   0.257937
# G1                   0.144308
# absences             0.069144
# goout                0.032323
# Walc                 0.030536
# health               0.028308
# Medu                 0.027702
# freetime             0.026866
# Fedu                 0.026596
# famrel               0.026042
# studytime            0.023256
# failures             0.020352
# Dalc                 0.017034
# traveltime           0.016919
# romantic_yes         0.014519
# activities_yes       0.014405
# paid_yes             0.014361
# sex_M                0.013956
# nursery_yes          0.013445
# Mjob_other           0.012623
# Mjob_services        0.012165
# famsup_yes           0.011963
# reason_reputation    0.011915
# famsize_LE3          0.011518
# internet_yes         0.011432
# Fjob_other           0.011089
# reason_home          0.010877
# address_U            0.010514
# guardian_mother      0.010161
# Fjob_services        0.010111
# schoolsup_yes        0.008898
# Pstatus_T            0.008090
# Fjob_teacher         0.008065
# Mjob_health          0.007614
# Mjob_teacher         0.006131
# guardian_other       0.006056
# reason_other         0.004761
# higher_yes           0.004548
# Fjob_health          0.003459
# =============================================================================

###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3   
 
# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 72.73

###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3   
 
# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 72.73


###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 평일/주말 술
std['Dalc/Walc'] = list(map(lambda x, y : x / y, std.Dalc, std.Walc))

# 변수 삭제
std = std.drop(['Dalc','Walc'],axis = 1)

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3   
 
# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 69.7

###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 아빠는 딸, 엄마는 아들 학력 + 직업
edu_according_gender = []
job_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        edu_according_gender.append(std.loc[i,'Fedu'])
        job_according_gender.append(std.loc[i,'Fjob'])
    else :
        edu_according_gender.append(std.loc[i,'Medu'])
        job_according_gender.append(std.loc[i,'Mjob'])
        
std['edu_according_gender'] = edu_according_gender
std['job_according_gender'] = job_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu','Mjob','Fjob'],axis = 1)

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3  


# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 69.69


###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 친구/자유 
std['goout/freetime'] = list(map(lambda x, y : x / y, std.goout, std.freetime))

# 변수 삭제
std = std.drop(['goout','freetime'],axis = 1)

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3  


# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 71.72

###############################################################################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3  


# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 70.7

##################
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 부모 직업 선생이면 1 아니면 0
std.Mjob = np.where(std.Mjob == 'teacher', 1, 0)
std.Fjob = np.where(std.Fjob == 'teacher', 1, 0)
std.reason = np.where(std.reason == 'course', 1, np.where(std.reason == 'reputation', 1, 0))

# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3  


# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 67.68










# 매개변수 튜닝
vscore_te = []
for i in range(2, 101) :
    m_rf = rf(n_estimators = i, random_state = 0)
    m_rf.fit(train_x, train_y)
    vscore_te.append(m_rf.score(test_x, test_y)) 

vscore_te = Series(vscore_te, index = range(2,101))
vscore_te.sort_values(ascending = False)    # n_estimators = 23 일때, 76.77
                                            

# 교차검증
vscore_te2 = []
for i in range(2, 101) :
    m_rf = rf(n_estimators = i, random_state = 0)
    vscore = cross_val_score(m_rf, std2.loc[:,std2.columns != 'G3'], std2.G3, cv = 5)
    vscore_te2.append(vscore.mean())  

vscore_te2 = Series(vscore_te2, index = range(2,101))
vscore_te2.sort_values(ascending = False)   # n_estimators = 59 일때, 76.2


####################################################################################
# 직업 +  g2/g1추가 + higher + Pstatus 제외 75.76
# 파일 불러오기
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x * y, g2, g1))


# 변수 변형
std2 = pd.get_dummies(std, drop_first = True)

# Y값 범주화
vG3 = []
for i in std2.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std2.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std2.loc[:,std2.columns != 'G3'],
                                                    std2.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  

# 변수 중요도
s2 = Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = True)


# =============================================================================
# G2                   0.275827
# G1                   0.150676
# g2/g1                0.082579
# absences             0.070167
# goout                0.027660
# Walc                 0.025794
# health               0.025475
# famrel               0.025264
# according_gender     0.024698
# freetime             0.022822
# studytime            0.022008
# failures             0.017056
# Dalc                 0.015817
# activities_yes       0.013171
# nursery_yes          0.012783
# traveltime           0.012623
# paid_yes             0.012279
# address_U            0.011244
# famsize_LE3          0.011146
# romantic_yes         0.010098
# reason_reputation    0.009744
# famsup_yes           0.009651
# Mjob_other           0.009630
# Fjob_other           0.009625
# Fjob_services        0.009108
# internet_yes         0.008890
# Mjob_teacher         0.008888
# Mjob_services        0.008734
# reason_home          0.008501
# schoolsup_yes        0.008161
# guardian_mother      0.007962
# Fjob_teacher         0.007865
# Mjob_health          0.007375
# reason_other         0.006321
# guardian_other       0.005676
# Fjob_health          0.004684
# =============================================================================







####################################################################################

# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 양육자 부모님이면 1 아니면 0   69.7
std.guardian = np.where(std.guardian == 'mother', 1, np.where(std.guardian == 'father', 0, 0)) 

####################################################################################

std.schoolsup = np.where(std.schoolsup == 'yes', 1, 0)
std.famsup = np.where(std.famsup == 'yes', 1, 0)
std.activities = np.where(std.activities == 'yes', 1, 0)
std.nursery = np.where(std.nursery == 'yes', 1, 0)


std['plus'] = std.iloc[:, [9,10,12,13]].sum(1)

std = std.drop(['schoolsup', 'famsup', 'activities', 'nursery'],axis = 1)

####################################################################################
# 교호작용(interaction)

m_poly = poly(4)
m_poly.fit(train_x)
train_x_poly = m_poly.transform(train_x)
test_x_poly  = m_poly.transform(test_x)


col_poly = m_poly.get_feature_names(array(std.loc[:,std.columns != 'G3'].columns))

DataFrame(train_x_poly, columns = col_poly)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_poly, train_y)
m_rf.score(test_x_poly, test_y)  # 68.68


Series(m_rf.feature_importances_, index = col_poly).sort_values(ascending = False)



# G1 G2^2 g2/g1                           0.006895

##############################################################################################################################
# 라벨인코더
from sklearn.preprocessing import LabelEncoder

for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3   
 
# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 68.68

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)


####################################################################################
# 직업 +  g2/g1추가 + higher + Pstatus 제외 75.76
# 파일 불러오기
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))

# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 75.76  

# 변수 중요도

 
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)


####################################################################################
# 직업 + inter g2/g1추가 + higher + Pstatus 제외 77.78
# 파일 불러오기
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['inter'] = list(map(lambda x, y, z : x * (y**2) * z, std.G1, std.G2, std['g2/g1']))


# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 75.76  

# 변수 중요도

 
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)


####################################################################################
# 직업 + g1^2g2^2 g2/g1추가 + higher + Pstatus 제외 75.76 스캘링
# 파일 불러오기
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['g1g2'] = list(map(lambda x, y : (x**2) * (y**2), g2, g1))

std = std.drop(['G1','G2'],axis = 1)

# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 71.72


Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)

# scaling standard()
m_st = standard()
m_st.fit(train_x)
train_x_st = m_st.transform(train_x)
test_x_st = m_st.transform(test_x)

# scaling minmax()
m_mm = minmax()
m_mm.fit(train_x)
train_x_mm = m_mm.transform(train_x)
test_x_mm = m_mm.transform(test_x)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_st, train_y)
m_rf.score(test_x_st, test_y)  # 67.68


Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_mm, train_y)
m_rf.score(test_x_mm, test_y)  # 67.68


# 스캘링 standard
m_poly = poly(5)
m_poly.fit(train_x_st)
train_x_st_poly = m_poly.transform(train_x_st)
test_x_st_poly  = m_poly.transform(test_x_st)

col_poly = m_poly.get_feature_names(array(std.loc[:,std.columns != 'G3'].columns))

DataFrame(train_x_st_poly, columns = col_poly)  # G1^2 G2^2                                        0.005363

# 스캘링 minmax
m_poly = poly(5)
m_poly.fit(train_x_mm)
train_x_mm_poly = m_poly.transform(train_x_mm)
test_x_mm_poly  = m_poly.transform(test_x_mm)

col_poly = m_poly.get_feature_names(array(std.loc[:,std.columns != 'G3'].columns))

DataFrame(train_x_mm_poly, columns = col_poly)  # G1^2 G2^2                                        0.005363

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_st_poly, train_y)
m_rf.score(test_x_st_poly, test_y)  # 71.72


Series(m_rf.feature_importances_, index = col_poly).sort_values(ascending = False)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_mm_poly, train_y)
m_rf.score(test_x_mm_poly, test_y)  # 71.72


Series(m_rf.feature_importances_, index = col_poly).sort_values(ascending = False)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_mm, train_y)
m_rf.score(test_x_mm, test_y)  # 77.78 

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)


####################################################################################
# 직업 + absences범주화 g2/g1추가 + higher + Pstatus 제외 75.76       6 7 9 
# 파일 불러오기
run profile1
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))

# absences범주화
vab = []
for i in std.absences :
    if i <= 4 :
        vab.append(0)
    elif i <= 6 :
        vab.append(1)
    elif i <= 8 :
        vab.append(2)    
    elif i <= 10 :
        vab.append(3)        
    else :
        vab.append(4)

std.absences = vab


# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 99)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(train_x, train_y)
m_rf.score(test_x, test_y)  # 75.76  

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)



####################################################################################
# 직업 + absences범주화 + inter g2/g1추가 + higher + Pstatus 제외 78.79

# 파일 불러오기
run profile1
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['inter'] = list(map(lambda x, y, z : x * (y**2) * z, std.G1, std.G2, std['g2/g1']))


# absences범주화
vab = []
for i in std.absences :
    if i <= 4 :
        vab.append(0)
    elif i <= 6 :
        vab.append(1)
    elif i <= 8 :
        vab.append(2)    
    elif i <= 10 :
        vab.append(3)        
    else :
        vab.append(4)

std.absences = vab


# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 78.79  

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)

# 튜닝
vscore = []
for i in range(2, 101) :
    m_rf = rf(random_state = 0, n_estimators = i)
    m_rf.fit(train_x, train_y)
    vscore.append(m_rf.score(test_x, test_y))  # 78.79  

sorted(vscore)



####################################################################################
# 직업 + absences범주화 + inter g2/g1추가 + higher + Pstatus 제외 f/s 79.8

# 파일 불러오기
run profile1
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['inter'] = list(map(lambda x, y, z : x * (y**2) * z, std.G1, std.G2, std['g2/g1']))

# 자유/공부
std['f/s'] = list(map(lambda x, y : x / y, std.freetime, std.studytime))
std = std.drop(['freetime','studytime'], axis = 1)

# absences범주화
vab = []
for i in std.absences :
    if i <= 4 :
        vab.append(0)
    elif i <= 6 :
        vab.append(1)
    elif i <= 8 :
        vab.append(2)    
    elif i <= 10 :
        vab.append(3)        
    else :
        vab.append(4)

std.absences = vab


# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 78.79  

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)

# 튜닝
vscore = []
for i in range(2, 101) :
    m_rf = rf(random_state = 0, n_estimators = i)
    m_rf.fit(train_x, train_y)
    vscore.append(m_rf.score(test_x, test_y))  # 78.79  

sorted(vscore)

# 교차검증
vscore_te2 = []
for i in range(2, 101) :
    m_rf = rf(n_estimators = i, random_state = 0)
    vscore = cross_val_score(m_rf, std.loc[:,std.columns != 'G3'], std.G3, cv = 5)
    vscore_te2.append(vscore.mean())  

vscore_te2 = Series(vscore_te2, index = range(2,101))
vscore_te2.sort_values(ascending = False)   # n_estimators = 64 일때, 77.2

####################################################################################
# 직업 + absences범주화 + inter g2/g1추가 + higher + Pstatus 제외 + gogout범주화 f/s 80.8

# 파일 불러오기
run profile1
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus','address','schoolsup','famsize'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['inter'] = list(map(lambda x, y, z : x * (y**2) * z, std.G1, std.G2, std['g2/g1']))

# 자유/공부
std['f/s'] = list(map(lambda x, y : x / y, std.freetime, std.studytime))
std = std.drop(['freetime','studytime'], axis = 1)

# absences범주화
vab = []
for i in std.absences :
    if i <= 4 :
        vab.append(0)
    elif i <= 6 :
        vab.append(1)
    elif i <= 8 :
        vab.append(2)    
    elif i <= 10 :
        vab.append(3)        
    else :
        vab.append(4)

std.absences = vab

# goout범주화
vgo = []
for i in std.goout :
    if i <= 3 :
        vgo.append(0)     
    else :
        vgo.append(1)

std.goout = vgo


# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(test_x, test_y)  # 78.79  

# 변수 중요도
Series(m_rf.feature_importances_, index = train_x.columns).sort_values(ascending = False)

# 튜닝
vscore = []
for i in range(2, 101) :
    m_rf = rf(random_state = 0, n_estimators = i)
    m_rf.fit(train_x, train_y)
    vscore.append(m_rf.score(test_x, test_y))  # 78.79  

sorted(vscore)


# 교차검증
vscore_te2 = []
for i in range(2, 101) :
    m_rf = rf(n_estimators = i, random_state = 0)
    vscore = cross_val_score(m_rf, std.loc[:,std.columns != 'G3'], std.G3, cv = 5)
    vscore_te2.append(vscore.mean())  

vscore_te2 = Series(vscore_te2, index = range(2,101))
vscore_te2.sort_values(ascending = False)   # n_estimators = 70 일때, 77.2

####################################################################################
# 직업 +  inter g2/g1추가 + higher + Pstatus 제외 + goout범주화

# 파일 불러오기
run profile1
std = pd.read_csv('student_grade.csv')
std = std.drop(['higher','Pstatus'],axis = 1)
std = std.drop(['higher','Pstatus','address','schoolsup','famsize'],axis = 1)

# 아빠는 딸, 엄마는 아들 학력
v_according_gender = []
for i in range(0, len(std)) :
    if std.loc[i,'sex'] == 'F' :
        v_according_gender.append(std.loc[i,'Fedu'])
    else :
        v_according_gender.append(std.loc[i,'Medu'])
        
std['according_gender'] = v_according_gender

# 변수 삭제
std = std.drop(['sex','Medu','Fedu'],axis = 1)

# g2/g1 
g1 = list(map(lambda x : x + 1, std.G1))
g2 = list(map(lambda x : x + 1, std.G2))
std['g2/g1'] = list(map(lambda x, y : x / y, g2, g1))
std['inter'] = list(map(lambda x, y, z : x * (y**2) * z, std.G1, std.G2, std['g2/g1']))

# goout범주화
vgo = []
for i in std.goout :
    if i <= 3 :
        vgo.append(0)     
    else :
        vgo.append(1)

std.goout = vgo

# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])


# Y값 범주화
vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3  

# train과 test 나누기
train_x, test_x, train_y, test_y = train_test_split(std.loc[:,std.columns != 'G3'],
                                                    std.G3,
                                                    random_state = 0)

# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x, train_y)
m_rf.score(train_x, train_y)    # 1.0
m_rf.score(test_x, test_y)      # 78.79
    
# 교차검증
vscore_te2 = []
for i in range(2, 101) :
    m_rf = rf(n_estimators = i, random_state = 0)
    vscore = cross_val_score(m_rf, std.loc[:,std.columns != 'G3'], std.G3, cv = 5)
    vscore_te2.append(vscore.mean())  

vscore_te2 = Series(vscore_te2, index = range(2,101))
vscore_te2.sort_values(ascending = False)   # n_estimators = 69 일때, 79.24

m_rf = rf(n_estimators = 69, min_samples_split = 2, max_features = 8, random_state = 0)

vscore = cross_val_score(m_rf, std.loc[:,std.columns != 'G3'], std.G3, cv = 5)
vscore.mean()

####################################################################################
# 교호작용(interaction)

# famrel G1 G2^2
m_poly = poly(4)
m_poly.fit(train_x)
train_x_poly = m_poly.transform(train_x)
test_x_poly  = m_poly.transform(test_x)


col_poly = m_poly.get_feature_names(array(std.loc[:,std.columns != 'G3'].columns))

DataFrame(train_x_poly, columns = col_poly)


# random forest 모델 적용
m_rf = rf(random_state = 0)
m_rf.fit(train_x_poly, train_y)
m_rf.score(test_x_poly, test_y)  # 68.68


s1 = Series(m_rf.feature_importances_, index = col_poly).sort_values(ascending = False)[0:10]
s2 = s1.sort_values(ascending = True)
import seaborn as sns

plt.barh(s2.index.values, s2,  height=0.5, align='edge', linewidth=2, color = sns.color_palette("rocket_r", 40))
plt.rc('font', size = 12)
plt.rc('font',family='Malgun Gothic')  
plt.xlabel('특성 중요도', fontsize = 15)
  


# G1 G2^2 g2/g1                           0.006895
############### 시각화
# 파일 불러오기
std = pd.read_csv('student_grade.csv')

# 변수 변형
for i in std.columns.values :
    if str(std.loc[0,i]).isnumeric() :
        pass
    else :
        m_label = LabelEncoder()
        m_label.fit(std.loc[:,i])
        std.loc[:,i] = m_label.transform(std.loc[:,i])

vG3 = []
for i in std.G3 :
    if i >= 18 :
        vG3.append('A')
    elif i >= 15 :
        vG3.append('B')
    elif i >= 12 :
        vG3.append('C')
    elif i >= 9 :
        vG3.append('D')        
    elif i >= 6 :
        vG3.append('E')
    elif i >= 3 :
        vG3.append('F')
    else :
        vG3.append('G')
   
std.G3 = vG3     
std = std.sort_values('G3')
std.shape
std.index = Series(np.arange(0,395))
std2 = std.groupby('G3').mean().iloc[:,0:28]

std.groupby('G3').mean().loc[:,'absences']
std.groupby('G3').mean().loc[:,'nursery']
std.groupby('G3').mean().loc[:,'goout']
std.groupby('G3').mean().loc[:,'Walc']
std.groupby('G3').mean().loc[:,'Dalc']
plt.figure()
std.goout.plot(style = '.')


####
# 그리드
m_rf = rf(random_state = 0)
v_params = {'min_samples_split' : np.arange(2,21), 
            'max_features' : np.arange(1,26),
            'n_estimators' : np.arange(10,71)}

# 2-2) 그리드 서치 모델 생성
m_grid = GridSearchCV(m_rf,        # 적용 모델
                      v_params,    # 매개변수 조합(딕셔너리)
                      cv=5)

m_rf = rf(n_estimators = 69, min_samples_split = 2, max_features = 8, random_state = 0)
vscore = cross_val_score(m_rf, std.loc[:,std.columns != 'G3'], std.G3, cv = 5)
vscore.mean()
# 2-3) 그리드 서치에 의한 모델 학습
m_grid.fit(train_x, train_y)

# 2-4) 결과 확인
m_grid.best_score_    # 82.11   81.77
m_grid.best_params_   # {'max_features': 8, 'min_samples_split': 2} 


df_result = DataFrame(m_grid.cv_results_)

# 2-5) 최종 평가
m_grid.score(test_x, test_y)

# 2-6) 그리드 서치 결과 시각화
df_result.mean_test_score      # 교차 검증의 결과(5개의 점수에 대한 평균)
arr_score = np.array(df_result.mean_test_score).reshape(25, 19)

import mglearn
plt.rc('figure', figsize=(10,10))
plt.rc('font', size=6)

mglearn.tools.heatmap(arr_score,                      # 숫자 배열
                      'min_samples_split',            # x축 이름(컬럼)
                      'max_features',                 # y축 이름(행)
                      v_params['min_samples_split'],  # x축 눈금
                      v_params['max_features'],       # y축 눈금
                      cmap='viridis')

