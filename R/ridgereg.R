#' Ridge regression with QR
#'
#' @field formula formula.
#' @field data data.frame.
#' @field lambda numeric.
#' @field parsedata character.
#' @field names vector.
#' @field beta numeric.
#' @field data_n character.
#'
#' @export ridgereg
#' @exportClass ridgereg
#'
#'
ridgereg <- setRefClass( Class = "ridgereg",
                         fields = list(
                           formula = "formula",
                           data = "data.frame",
                           lambda= "numeric",
                           parsedata = "character",
                           names="vector",
                           beta= "numeric",
                           data_n= "character",
                           y_hat= "matrix"



                         ),
                         methods = list(
                           initialize= function(formula, data, lambda){
                             stopifnot (class(formula) == "formula")
                             stopifnot (class(data) == "data.frame")
                             formula <<- formula
                             data<<- data
                             lambda<<- lambda
                             Xx= model.matrix(formula, data)
                             names<<-c(colnames(Xx))
                             data_n <<- deparse(substitute(data))
                             std= c(1)
                             x_mean= c(1)
                             for (i in 2:ncol(Xx)){
                               std[i] <- sd(Xx[,i])
                               x_mean[i] <- mean(Xx[,i])

                             }
                             X= scale(Xx[,-1])
                             X= cbind(Xx[1],X)
                             X <- matrix(as.double(X), ncol = ncol(X))

                             y= as.matrix(data[,all.vars(expr = formula)[1]])
                             y_mean= mean(y)
                             y= y-y_mean
                             y= c(y, rep(0, ncol(X)))
                             X= rbind(X, sqrt(lambda)*diag(ncol(X)))

                             QR= qr(X)
                             Q= qr.Q(QR)
                             R= qr.R(QR)

                             beta<<- (qr.coef(QR, y))/std
                             beta[1] <<- beta[1] - as.numeric(beta[-1] %*% x_mean[-1]) + y_mean

                             y_hat <<- Xx %*% beta



                           },
                           print= function(){


                             cat("\n","Call:","\n",
                                 paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
                                       ", ", "data = ", data_n, ", lambda = ", lambda,")",sep = "", collapse = "\n" ),
                                 "\n","Coefficients:","\n",
                                 format(names, justify = "centre",width = 12),"\n",
                                 format(round(beta,2), justify = "centre",width = 13))

                           },
                           predict = function(){
                             return(y_hat)
                           },
                           coef = function(){
                             a <- as.vector(beta)
                             names(a) <- names
                             return(a)
                           }

                         )

)

# #
# ridgereg1<- ridgereg$new(Petal.Length ~ Species,data=iris, lambda=1.2)
# formula= Petal.Length ~ Species
# data=iris
#
# # lm.ridge(formula, data, lambda = 1)
# #
# ridgereg1 <- ridgereg$new(Petal.Length ~ Species,data=iris, lambda=1.2)
# ridgereg1$print()
# ridgereg1$coef()
# ridgereg1$predict()
