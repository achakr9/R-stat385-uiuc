## Monday class:
set.seed(385)
nums <- sample(x = 1:100, size = 5, replace = TRUE)
sum(nums)

# for loop
sum <- 0
for (i in 1:length(nums)) {
  sum <- sum + nums[i]
}
sum

# i will subset value in nums automatically. less work:
sum2 <- 0
for (i in nums) {
  sum2 <- sum2 + i
}
sum2

## LAB 3
## Exercise 1
# PLANTGROWTH DATASET
PlantGrowth
table(PlantGrowth$group)

hist(x = PlantGrowth$weight, main = 'Histogram of the Plants Dried Weight', 
     xlab = 'Dried Weight (lbs)', ylab = 'Density', breaks = 15)

boxplot(formula = weight ~ group, data = PlantGrowth, main = "Boxplot of Plants' Dried Weights vs. Treatments", 
        xlab = 'Treatment Group', ylab = "Dried Weight (lbs)", col = c("darkorange", "red3", "dodgerblue"))

## TREES DATASET
trees
plot(formula = Height ~ Girth, data = trees, main = "Black Cherry Trees Girth vs. Height", 
     xlab = "Girth (inches)", ylab = "Height (ft)", pch = 16, col = "dodgerblue")

fivenum(trees$Girth)
trees$Group <- cut(x = trees$Girth, breaks = fivenum(trees$Girth), right = FALSE, include.lowest = TRUE, 
                   labels = c("group1", "group2", "group3", "group4"))
trees

boxplot(formula = Height ~ Group, data = trees, main = "Boxplot of Girth vs Height", 
        xlab = "Girth Group", ylab = "Height (ft)", col = c("magenta3", "seagreen3", "dodgerblue3", "darkgray"))

## WARPBREAKS DATASET
str(warpbreaks)
table(warpbreaks$wool, warpbreaks$tension)

hist(x = warpbreaks$breaks, main = 'Histogram of Number of Breaks', xlab = "Number of Breaks", 
     ylab = "Density", breaks = 12)

boxplot(formula = breaks ~ tension, data = warpbreaks, main = "Boxplot of Number of Breaks vs Tension",
        xlab = "Tension", ylab = "Number of Breaks", col = c("darkred", "darkgray", "darkorange"))

boxplot(formula = breaks ~ wool, data = warpbreaks, main = "Boxplot of Number of Breaks vs Wool",
        xlab = "Wool Type", ylab = "Number of Breaks", col = c("darkolivegreen", "darkseagreen"))

boxplot(formula = breaks ~ wool*tension, data = warpbreaks, main = "Boxplot of Number of Breaks vs Wool",
        xlab = "Wool Type", ylab = "Number of Breaks", col = c("darkolivegreen", "darkseagreen"))

