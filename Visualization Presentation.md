# Analyzing Fictional Employee Data

Note: This data was generated by IBM and is not reflective of any real organization.

This dataset, [IBMEmployeeAttrition](https://github.com/Madison-Bunting/DSCI-304/blob/main/IBMEmployeeAttrition.csv), is about attrition, which is the rate at which employees are leaving a company. It was originally hosted on IBM's website, but has since been taken down, however it can be found on Kaggle [here](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset). 

Our goal is to identify why employees are no longer with this company, meaning we will use "attrition" as the target variable.
The dataset has 35 variables for each of 1470 employees as detailed below.

- **Numeric variables**: 
  - Personal information: age, commuting distance, employee ID
  - Income: hourly rate, daily rate, monthly rate, monthly income, percent salary hike
  - Employee's history with the company: years at company, years in current role, years since last promotion, years with curr manager, total working years
  - Other: number of companies the employee has worked for, number of hours per week they work, training time over the past year, employee count
- **Categorical variables**: 
  - **Binary variables**: attrition, gender, whether the employee is over 18, whether the employee is eligible for overtime
  - **Nominal variables**: department, education field, job role, marital status
  - **Ordinal variables**: 
    - Job satisfaction (scale of 1 = low, 2 = medium, 3 = high, 4 = very high): environment satisfaction, job involvement, job satisfaction
    - Work-Life Balance (scale of 1 = bad, 2 = good, 3 = better, 4 = best)
    - Relationship satisfaction (scale of 1 = low, 2 = medium, 3 = high, 4 = very high)
    - Performance rating (scale of 1 = low, 2 = good, 3 = excellent, 4 = outstanding)
    - Education (where 1 = below college, 2 = college, 3 = bachelor, 4 = master, 5 = doctor)
    - Other: business travel, job level, stock option level

First, we will examine the overall attrition rate in the below waffle chart.

![image](https://user-images.githubusercontent.com/89811204/204194330-a6120916-4dbe-4a38-9e8f-507470d61a24.png)

We observe that 16 out of every 100 employees have left. Companies should generally aim for an [attrition rate of 10% or lower](https://insightglobal.com/blog/employee-attrition-rate-how-to-calculate-improve/), depending on the industry. This suggests this company can work on their retention. Let's examine some of the reasons employees might be leaving. 
The attrition rates for each of the qualitative responses to some of the "survey questions" are visualized in the animation below.

![animation](https://user-images.githubusercontent.com/89811204/204196084-f0d071fd-b28e-4184-bc0d-ccd678947726.gif)

It appears that lower scores generally tend to have higher attrition rates, which is what we would expect since most people would prefer to not hate their workplace. Let's now delve into some potential reasons for those scores. During the return to office following the recovery from the COVID-19 pandemic, there have been numerous [articles discussing the "future of work"](https://doi-org.ezproxy.rice.edu/10.1080/13678868.2022.2047380) that articulate employees preference for at least a hybrid option, due to better work life balance without the commute. With this in mind, let's examine the relationship between work life balance rating and commute distance. 

![image](https://user-images.githubusercontent.com/89811204/204342639-29457164-11a2-4a85-a74e-4389eb56cec1.png)

We can see that employees who left were more likely to have longer commutes. 

Now let's look at the overall distribution of commute distances to get a sense of how far most employees live from work and if that affects the number leaving.

![image](https://user-images.githubusercontent.com/89811204/204196045-62b5a2e2-909e-4ef9-bde9-e01263cd10f2.png)

We see most people live within 10 miles of the office, and it appears more people are leaving that live closer to the office, however, it is important to note that this histogram reflects counts of employees who stayed and left, not the attrition rate. Next, let's examine the attrition rate of commute distances by generation [(age ranges defined by Pew Research Center)](https://www.pewresearch.org/fact-tank/2019/01/17/where-millennials-end-and-generation-z-begins/).

![image](https://user-images.githubusercontent.com/89811204/204587477-9e2e825d-7194-4596-a75b-c9cfd6831e53.png)

This [survival analysis graph](https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html) suggests that most people who are leaving are younger. This makes sense because younger people are earlier in their careers, so they have fewer attachments or loyalty to the company, particularly with Millennials and Generation Z. 
Additionally, the national [average age at a person's first marriage](https://www.census.gov/content/dam/Census/library/visualizations/time-series/demo/families-and-households/ms-2.pdf) has been increasing for over 20 years and is now around 29 years of age. This means the majority of young people are single, so they also have fewer commitments both to their community and at-home responsibilities, which makes it easier for young people to move to find better opportunities. 

So far, we know employees that tend to leave at higher rates are younger and have long commutes. Could there be something deeper motivating this company's attrition problem? Let's see if there are any trends of attrition by department.

![image](https://user-images.githubusercontent.com/89811204/204597245-894513e4-174a-4173-958f-acf4c8bd245b.png)

The effect plot above demonstrates that although Sales has a slightly higher attrition rate, Human Resources has a much wider range. This is particularly an issue when there are only 63 few members of HR at this company in the first place, as HR plays an essential role in avoiding and managing employee turnover - if they don't want to stay, who else will?

To wrap up, I will offer a few recommendations to help this company reduce their attrition rate, correlated with the key findings above:
- Improve employee satisfaction by offering a hybrid working environment, and help make the commute more managable by [providing transportation](https://www.sciencetimes.com/articles/38186/20220613/inside-googles-bus-fleet-a-tour-of-the-companys-transportation-options.htm#:~:text=These%20shuttle%20buses%20are%20equipped,cities%20the%20shuttle%20travels%20through.) or vouchers for ridesharing. This will also allow employees to get work done on the road
- Design initiatives to improve retention of younger employees, like giving recognition and expanding career opportunities, improve feedback mechanisms, and allow them to see their ideas being implemented
- Create consistency within the HR department and build more robust onboarding and training as well as events to build company culture

In the future, it would be interesting to analyze a similar dataset which includes more demographic information, like racial/ethnic data or more diverse gender identities, as these are particularly pressing in the current cultural moment. 
