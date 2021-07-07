# 데이터 불러오기(https://www.kaggle.com/kwadwoofosu/predict-test-scores-of-students)
import pandas as pd
data = pd.read_csv('test_scores.csv')

# 데이터 설명
# =============================================================================
# school - Name of the school the student is enrolled in - 학교 이름
# school_setting - The location of the school - 학교 지역
# school_type - The type of school. Either public or non-public - 공립 및 사립
# classroom - The type of classroom - 반
# teaching_method - Teaching methods: Either experimental or Standard - 교육법 : 실험 및 표준
# n_student - Number of students in the class - 학급의 학생수
# student_id - A unique ID for each student - 학생 id
# gender - The gender of the students: male or female - 성별
# lunch - Whether a student qualifies for free/subsidized lunch or not - 무료 혹은 지원 급식 유무
# pretest - The pretest score of the students out of 100 - 이전 성적
# posttest - 현재 성적
# =============================================================================

# 결측치 확인
import missingno as msno
msno.matrix(data)

# 이상치 확인
sum(data.pretest > 100)
sum(data.pretest < 0)
sum(data.posttest > 100)
sum(data.posttest < 0)

# 데이터 확인
data.head()
data.info()
data.describe()

for i in range(len(data.columns)) :
    print('칼럼명 {0} 안에는 {1}값들이 있습니다. \n'.format(data.columns[i], data.iloc[:, i].unique()))
    
# EDA
import scipy.stats  # 통계적 검증 모듈
from scipy.stats import f_oneway

# 가설1 - 성별에 따른 뚜렷한 점수차이가 존재할까?
gender_score = data.groupby('gender')['posttest'].mean()
gender_score.plot(kind = 'bar', rot = 0, color = ['red', 'blue'])

scipy.stats.ttest_ind(data.loc[data.gender == 'Female', 'posttest'], data.loc[data.gender == 'Male', 'posttest'], equal_var = False)

# =============================================================================
# gender
# Female    67.004735
# Male      67.197772
#
# Ttest_indResult(statistic=-0.3185546435336617, pvalue=0.7500955214764365) : 차이가 없다.
# =============================================================================

# 가설2 - 학교가 공립/사립에 따른 점수차이가 존재할까?
school_type_score = data.groupby('school_type')['posttest'].mean()
school_type_score.plot(kind = 'bar', rot = 0, color = ['red', 'blue'])

scipy.stats.ttest_ind(data.loc[data.school_type == 'Non-public', 'posttest'], data.loc[data.school_type == 'Public', 'posttest'], equal_var = False)

# =============================================================================
# school_type
# Non-public    75.961887
# Public        64.016435
#
# Ttest_indResult(statistic=21.096177470583047, pvalue=1.1463299450933866e-84) : 사립인 학교가 성적이 높다.
# =============================================================================

# 가설3 - 무료/지원 급식에 따른 점수차이가 존재할까?
lunch_score = data.groupby('lunch')['posttest'].mean()
lunch_score.plot(kind = 'bar', rot = 0, color = ['red', 'blue'])

scipy.stats.ttest_ind(data.loc[data.lunch == 'Does not qualify', 'posttest'], data.loc[data.school_type != 'Does not qualify', 'posttest'], equal_var = False)

# =============================================================================
# lunch
# Does not qualify                    74.375309
# Qualifies for reduced/free lunch    57.476035
#
# Ttest_indResult(statistic=16.39284248372113, pvalue=6.962812009317858e-58) : 급식을 지원 받지 않는 학생이 성적이 높다.
# =============================================================================

# 가설4 - 학교 지역에 따른 점수차이가 존재할까?
school_setting_score = data.groupby('school_setting')['posttest'].mean()
school_setting_score.plot(kind = 'bar', rot = 0, color = ['red', 'yellow', 'blue'])

f_oneway(data.loc[data.school_setting == 'Rural', 'posttest'], data.loc[data.school_setting == 'Suburban', 'posttest'], data.loc[data.school_setting == 'Urban', 'posttest'])

# =============================================================================
# school_setting
# Rural       64.050980
# Suburban    76.037657
# Urban       61.748344
#
# F_onewayResult(statistic=284.6449099827679, pvalue=2.776785026022543e-110) : 학교 지역으로 인한 평균적 차이가 난다. ---> 사후검증
# =============================================================================

# 가설5 - 교육법에 따른 점수차이가 존재할까?
teaching_method_score = data.groupby('teaching_method')['posttest'].mean()
teaching_method_score.plot(kind = 'bar', rot = 0, color = ['red', 'blue'])

scipy.stats.ttest_ind(data.loc[data.teaching_method == 'Experimental', 'posttest'], data.loc[data.teaching_method == 'Standard', 'posttest'], equal_var = False)

# =============================================================================
# teaching_method
# Experimental    72.982895
# Standard        63.847050
#
# Ttest_indResult(statistic=14.981069631474819, pvalue=2.194151774179099e-47) : Experimental교육법을 받은 학생들의 성적이 높다.
# =============================================================================

# 가설6 - 학급 수에 따른 점수차이가 존재할까?
class_fm = []

for i in data['n_student'] :
    if i > data['n_student'].mean() :
        class_fm.append('m')
    else :
        class_fm.append('f')
        
data.groupby(class_fm)['posttest'].mean()    

scipy.stats.ttest_ind(data.loc[data.n_student > data['n_student'].mean(), 'posttest'], data.loc[data.n_student < data['n_student'].mean(), 'posttest'], equal_var = False)

# =============================================================================
# f    72.454545
# m    61.038000
# 
# Ttest_indResult(statistic=-20.45216936457601, pvalue=1.207053114109686e-84) : 학급수가 적을 수록 학생들의 성적이 높다.
# =============================================================================

# 상관관계 알아보기
data_corr = data.drop(['school', 'student_id', 'classroom', 'student_id'], axis = 1)
data_corr.columns  # 'school_setting', 'school_type', 'teaching_method', 'gender', 'lunch'는 범주형변수이며, 
                   # 순위변수가 아닌 명목변수이므로 One-Hot Encoding 진행 결정

## One-Hot Encoding
data_corr = pd.get_dummies(data_corr)

## 상관관계 heatmap 시각화
import seaborn as sns
corr = data_corr.corr()
sns.heatmap(corr, annot = True)

# =============================================================================
# - pretest와 높은 양의 상관관계를 띔
# - 점심지원을 받지 않는 학생과 중간 양의 상관관계를 띔
# - 점심지원을 받는 학생과 중간 음의 상관관계를 띔
# =============================================================================

# 데이터 분리
from sklearn.model_selection import train_test_split
X = data_corr.drop('posttest', axis = 1)
Y = data_corr['posttest']

train_x, test_x, train_y, test_y = train_test_split(X, Y, train_size = 0.7, random_state = 2021)  # stratify : 분류모델에서 중요(데이터가 쏠리는 것을 방지)

# 기초 회귀 모델 적용
from sklearn.linear_model import LinearRegression
m_reg = LinearRegression()
dir(LinearRegression())
m_reg.fit(train_x, train_y)
m_reg.score(train_x, train_y)  # 0.9468366015910417
m_reg.score(test_x, test_y)  # 0.9481963717012319  # 과적합X

# 회귀분석 유의성 검정 결과 확인
import statsmodels.api as sm
sm_reg = sm.OLS(train_y, train_x).fit()
print(sm_reg.summary())

# Ridge
from sklearn.linear_model import Ridge

m_ridge = Ridge()
m_ridge.fit(train_x, train_y)
m_ridge.score(train_x, train_y)  # 0.9468364943282811
m_ridge.score(test_x, test_y)  # 0.9481943742111022

## 매개변수 튜닝
alpha = [0.001, 0.01, 0.1, 1, 10, 100]

for i in alpha :
    m_ridge = Ridge(alpha = i)
    m_ridge.fit(train_x, train_y)
    print('alpha가 {0}일 때, train 점수 : {1} test 점수 : {2}'.format(i, m_ridge.score(train_x, train_y), m_ridge.score(test_x, test_y)))

# =============================================================================
# alpha가 0.001일 때, train 점수 : 0.946836601590934 test 점수 : 0.9481963698046538
# alpha가 0.01일 때, train 점수 : 0.9468366015802758 test 점수 : 0.9481963527263232
# alpha가 0.1일 때, train 점수 : 0.9468366005148163 test 점수 : 0.9481961810397688
# alpha가 1일 때, train 점수 : 0.9468364943282811 test 점수 : 0.9481943742111022
# alpha가 10일 때, train 점수 : 0.9468262233257254 test 점수 : 0.9481676585734752
# alpha가 100일 때, train 점수 : 0.9460610425409486 test 점수 : 0.9472933094413045
# =============================================================================

# Lasso
from sklearn.linear_model import Lasso

m_lasso = Lasso()
m_lasso.fit(train_x, train_y)
m_lasso.score(train_x, train_y)  # 0.9233608582824564
m_lasso.score(test_x, test_y)  # 0.9247942717120871

## 매개변수 튜닝
alpha = [0.001, 0.01, 0.1, 1, 10, 100]

for i in alpha :
    m_lasso = Lasso(alpha = i)
    m_lasso.fit(train_x, train_y)
    print('alpha가 {0}일 때, train 점수 : {1} test 점수 : {2}'.format(i, m_lasso.score(train_x, train_y), m_lasso.score(test_x, test_y)))
    
# =============================================================================
# alpha가 0.001일 때, train 점수 : 0.9468363954232446 test 점수 : 0.9481942389592274
# alpha가 0.01일 때, train 점수 : 0.9468186220343126 test 점수 : 0.9481553966656311
# alpha가 0.1일 때, train 점수 : 0.9458703590919117 test 점수 : 0.9470257980123039
# alpha가 1일 때, train 점수 : 0.9233608582824564 test 점수 : 0.9247942717120871
# alpha가 10일 때, train 점수 : 0.9000815575738701 test 점수 : 0.9014595574379091
# alpha가 100일 때, train 점수 : 0.6132080426397082 test 점수 : 0.6068086649379731 
# =============================================================================
    
# ElasticNet
from sklearn.linear_model import ElasticNet    
m_elasticnet = ElasticNet()    
m_elasticnet.fit(train_x, train_y)
m_elasticnet.score(train_x, train_y)  # 0.9255052077004441
m_elasticnet.score(test_x, test_y)  # 0.9273270861135228

## 매개변수 튜닝 방법1 - for문
l1_ratio = [0, 0.2, 0.4, 0.6, 0.8, 1]
for i in alpha :
    for j in l1_ratio :        
        m_elasticnet = ElasticNet(alpha = i, l1_ratio = j)
        m_elasticnet.fit(train_x, train_y)
        print('alpha가 {0}이고 l1_ratio가 {1}일 때, train 점수 : {2} test 점수 : {3}'.format(i, j, m_elasticnet.score(train_x, train_y), m_elasticnet.score(test_x, test_y)))
    
# =============================================================================
# alpha가 0.001이고 l1_ratio가 0일 때, train 점수 : 0.9468363630242198 test 점수 : 0.9481933162705285
# alpha가 0.001이고 l1_ratio가 0.2일 때, train 점수 : 0.9468364080326147 test 점수 : 0.9481934631056008
# alpha가 0.001이고 l1_ratio가 0.4일 때, train 점수 : 0.9468364316715651 test 점수 : 0.9481936857706768
# alpha가 0.001이고 l1_ratio가 0.6일 때, train 점수 : 0.9468364397457419 test 점수 : 0.9481938949285649
# alpha가 0.001이고 l1_ratio가 0.8일 때, train 점수 : 0.9468364369739288 test 점수 : 0.9481941591421237
# alpha가 0.001이고 l1_ratio가 1일 때, train 점수 : 0.9468363954232446 test 점수 : 0.9481942389592274
# alpha가 0.01이고 l1_ratio가 0일 때, train 점수 : 0.9468138737517562 test 점수 : 0.9481467381079112
# alpha가 0.01이고 l1_ratio가 0.2일 때, train 점수 : 0.946817614163048 test 점수 : 0.9481505780623077
# alpha가 0.01이고 l1_ratio가 0.4일 때, train 점수 : 0.9468198301771038 test 점수 : 0.9481545979321968
# alpha가 0.01이고 l1_ratio가 0.6일 때, train 점수 : 0.9468205154548356 test 점수 : 0.9481570163522913
# alpha가 0.01이고 l1_ratio가 0.8일 때, train 점수 : 0.9468195770008382 test 점수 : 0.9481577039612631
# alpha가 0.01이고 l1_ratio가 1일 때, train 점수 : 0.9468186220343126 test 점수 : 0.9481553966656311
# alpha가 0.1이고 l1_ratio가 0일 때, train 점수 : 0.9453308488231715 test 점수 : 0.9465403715786441
# alpha가 0.1이고 l1_ratio가 0.2일 때, train 점수 : 0.9455169962914565 test 점수 : 0.9467031339303578
# alpha가 0.1이고 l1_ratio가 0.4일 때, train 점수 : 0.945650306256164 test 점수 : 0.9468095963161205
# alpha가 0.1이고 l1_ratio가 0.6일 때, train 점수 : 0.9457164277016384 test 점수 : 0.9468472254456605
# alpha가 0.1이고 l1_ratio가 0.8일 때, train 점수 : 0.9457971195246487 test 점수 : 0.9469280341556471
# alpha가 0.1이고 l1_ratio가 1일 때, train 점수 : 0.9458703590919117 test 점수 : 0.9470257980123039
# alpha가 1이고 l1_ratio가 0일 때, train 점수 : 0.9263328992290086 test 점수 : 0.9286470533852108
# alpha가 1이고 l1_ratio가 0.2일 때, train 점수 : 0.925966317884929 test 점수 : 0.9280767419314762
# alpha가 1이고 l1_ratio가 0.4일 때, train 점수 : 0.9256915063285688 test 점수 : 0.9276081754057659
# alpha가 1이고 l1_ratio가 0.6일 때, train 점수 : 0.9252734868180702 test 점수 : 0.9270030760138201
# alpha가 1이고 l1_ratio가 0.8일 때, train 점수 : 0.9246002025495841 test 점수 : 0.9261588939341577
# alpha가 1이고 l1_ratio가 1일 때, train 점수 : 0.9233608582824564 test 점수 : 0.9247942717120871
# alpha가 10이고 l1_ratio가 0일 때, train 점수 : 0.9049742761636512 test 점수 : 0.9079749451008294
# alpha가 10이고 l1_ratio가 0.2일 때, train 점수 : 0.9008230457567765 test 점수 : 0.9028272770860499
# alpha가 10이고 l1_ratio가 0.4일 때, train 점수 : 0.9003451799523219 test 점수 : 0.9018001356393365
# alpha가 10이고 l1_ratio가 0.6일 때, train 점수 : 0.9002605866130249 test 점수 : 0.9016904033253907
# alpha가 10이고 l1_ratio가 0.8일 때, train 점수 : 0.9001727635905497 test 점수 : 0.9015769335079532
# alpha가 10이고 l1_ratio가 1일 때, train 점수 : 0.9000815575738701 test 점수 : 0.9014595574379091
# alpha가 100이고 l1_ratio가 0일 때, train 점수 : 0.7937330680331716 test 점수 : 0.7894199657653478
# alpha가 100이고 l1_ratio가 0.2일 때, train 점수 : 0.7691102458538817 test 점수 : 0.7638609747946394
# alpha가 100이고 l1_ratio가 0.4일 때, train 점수 : 0.7443704735537825 test 점수 : 0.7387689089658459
# alpha가 100이고 l1_ratio가 0.6일 때, train 점수 : 0.7124540551104999 test 점수 : 0.706517625879797
# alpha가 100이고 l1_ratio가 0.8일 때, train 점수 : 0.6703333190379219 test 점수 : 0.6641112927297306
# alpha가 100이고 l1_ratio가 1일 때, train 점수 : 0.6132080426397082 test 점수 : 0.6068086649379731
# =============================================================================

## 매개변수 튜닝 방법2 - 그리드 서치
from sklearn.model_selection import GridSearchCV

params = {'alpha' : [0.001, 0.01, 0.1, 1, 10, 100],
          'l1_ratio' : [0, 0.2, 0.4, 0.6, 0.8, 1]}

m_gird = GridSearchCV(m_elasticnet, params, cv = 5)
m_gird.fit(train_x, train_y)
m_gird.best_params_  # {'alpha': 0.01, 'l1_ratio': 1}
m_gird.best_score_  # 0.9460333058258209
m_gird.score(test_x, test_y)  # 0.9481553966656311
