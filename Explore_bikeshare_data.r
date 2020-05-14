
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Your solution code goes here
library(ggplot2)

# Clear added the Age column I add every time I run the code
ny = ny[,1:9]
chi = chi[,1:9]

# Add some derived 'Age' columns
year.of.data = 2017
ny = invisible(cbind(ny, Age=abs(ny$Birth.Year + (-year.of.data))))
chi = invisible(cbind(chi, Age=abs(chi$Birth.Year + (-year.of.data))))

# We only want to work with data that has a valid age and gender
ny_age = subset(ny, ny$Age > 0 & (ny$Gender == "Male" | ny$Gender == "Female"))
chi_age = subset(chi, chi$Age > 0 & (chi$Gender == "Male" | chi$Gender == "Female"))

# Summarize NY
print("--- NY ---")
summary(ny_age$Age)
by(ny_age$Age, ny_age$Gender, summary)

# Summarize CHI
print("--- CHI ---")
summary(chi_age$Age)
by(chi_age$Age, chi_age$Gender, summary)

# Summary shows strange maximum age
print("/n NY MIN BIRTH YEAR")
min(ny_age$Birth.Year)
print("/n CHI MIN BIRTH YEAR")
min(chi_age$Birth.Year)
# This verified that indeed somone entered their age as
# over 100 in the last six months

# Here I will transition into visualization
# Using "realistic ages" 10-80
max_age = 80
min_age = 10
ny_age = subset(ny, ny$Age > min_age & ny$Age < max_age & ny$Gender != "")
chi_age = subset(chi, chi$Age > min_age & chi$Age < max_age & chi$Gender != "")

# Plot
qplot(data=ny_age, x= ny_age$Age, binwidth=3, ylab="Count [#]",
    main="NY Age Dist. by Gender") +
    scale_x_discrete(limits=seq(min_age, max_age, 5), name="Age [years]") +
    facet_wrap(~Gender)
qplot(data=chi_age, x= chi_age$Age, binwidth=3, ylab="Count [#]",
    main="Chicago Age Dist. by Gender") +
    scale_x_discrete(limits=seq(min_age, max_age, 5), name="Age [years]") +
    facet_wrap(~Gender)

# Q1 SUMMARY
# (Used:
# [data summary, derived columns, filtering NA, univariate histogram plot, faceting])
# In summary, we can see very similar age distributions across both cities and gender
# groups.
# There is a very low user count in the teen and young 20s group.  The counts sharply
# pick up for
# mid and late 20s groups and peaks at around 30 or 35 pretty much consistent with
# the medians discovered
# in the summary stage.  The user count starts to really taper off after 40-45 yo.
# Really no crazy insight
# here as far as gender goes.  We basically see a unimodal right-skewed distribution
# of age among the users.

# THIS WILL TAKE A WHILE TO RUN
# Your solution code goes here
# Clear added the new column I add every time I run the code
# Not consider missing data
ny = ny[,1:9]
ny = subset(ny, !is.na(ny$Start.Time))
chi = chi[,1:9]
chi = subset(chi, !is.na(chi$Start.Time))
wash = wash[,1:7]
wash = subset(wash, !is.na(wash$Start.Time))

# Set the time label according to the hour the trip strated
# INPUT = date + time of the day in military time
# OUTPUT = 06-12:'MOR', 12-18:'AFT', 18-00:'EVE', 00-06:'NIGHT'
get_time_label <- function(time) {
    hour.of.day = as.numeric(substr(strsplit(toString(time), ":"), 15, 16))
    time.label = "NA"
    if (!is.na(hour.of.day)) {
        if (hour.of.day >= 6 & hour.of.day < 12) {
            time.label = "MOR"
        } else if (hour.of.day >= 12 & hour.of.day < 18) {
            time.label = "AFT"
        } else if (hour.of.day >= 18 & hour.of.day < 24) {
            time.label = "EVE"
        } else {
           time.label = "NIGHT"
        }
    }
    return(time.label)
}

# Add a time of day label TOD (Time Of Day)
ny = cbind(ny, TOD=NA)
chi = cbind(chi, TOD=NA)
wash = cbind(wash, TOD=NA)
samples = 1000
print(samples)
for (i in seq(from=1, to=samples, by=1)) { # Loop over times
    # Convert timestamp to hour of the day
    time = ny$Start.Time[i]
    time.label = get_time_label(time)
    ny[i, 10] = time.label # Update the TOD column
    time = chi$Start.Time[i]
    time.label = get_time_label(time)
    chi[i, 10] = time.label # Update the TOD column
    time = wash$Start.Time[i]
    time.label = get_time_label(time)
    wash[i, 8] = time.label # Update the TOD column
    if(counter %% (samples %/% 100) == 0) {
        print(samples %/% counter)
    }
}

# Validate labels
print("NY")
head(ny[,c(2, 10)], 10)
print("CHI")
head(chi[,c(2, 10)], 10)
print("WASH")
head(wash[,c(2, 8)], 10)

# Plot!
ny = subset(ny, ny$TOD != "NA")
qplot(data=ny, x= ny$TOD, geom="bar", ylab="Count [#]", xlab="TOD category",
      main="NY TOD Dist.")
chi = subset(chi, chi$TOD != "NA")
qplot(data=chi, x= chi$TOD, geom="bar", ylab="Count [#]", xlab="TOD category",
      main="CHI TOD Dist.")
wash = subset(wash, wash$TOD != "NA")
qplot(data=wash, x= wash$TOD, geom="bar", ylab="Count [#]", xlab="TOD category",
      main="WASH TOD Dist.")

# Q2 SUMMARY
# (Used:
# [conditional statements, loops, custom function refactoring, barplots])
# Interesting!  People in NY and Chicago are far less likely to take a
#night ride than people
# in DC.  Is that because of crime rates and the safety issue?  Is there a
#problem with our
# bikeshare service?  Who knows.  In both NY and Chicago afternoon rides are
# preferred whereas
# morning rides are preferred in DC.  Those distributions were worth
#investigating (unlike in my
# attempt in question 1) because that could help the company customize
# maintenance times for each
# city.  Sharing this data with government can help the city better
#understands transportation
# trends in their population.

# Your solution code goes here
ny = read.csv('new_york_city.csv')
ny = subset(ny, ny$Birth.Year > 1945 & ny$Birth.Year < 2000)
ggplot(aes(x=ny$Birth.Year, y=ny$Trip.Duration), data=ny,
  main="Trip Duration vs. Age") + ylab("time") +
  ylim(100, 5000) +
  geom_point(alpha=.075, color='Red', position = position_jitter(h=0)) +
  geom_line(stat = 'summary', fun.y=median) +
  scale_x_discrete(limits=seq(1930, 2020, 5), name="Year of Birth")

# Q2 SUMMARY (Used: [ggplot syntax, bi-variate analysis])
# Trip duration seems to not vary too much among age groups.  The thickness of
# the plot
# is consistent with previous findings where age was concentrated around 30 yo.

system('python -m nbconvert Explore_bikeshare_data.ipynb')
