
#' A linear regression package
#'
#' @field formula formula.
#' @field data data.frame.
#' @field dta character.
#' @field frmla formula.
#' @field X matrix.
#' @field y matrix.
#' @field Regressions_coefficients matrix.
#' @field The_fitted_values matrix.
#' @field The_residuals matrix.
#' @field The_degrees_of_freedom numeric.
#' @field The_residual_variance numeric.
#' @field The_variance_of_the_regression_coefficients matrix.
#' @field The_t_values matrix.
#'
#'
#' @export linreg
#' @exportClass linreg
#'
#' @examples
#'
#' linregtest <- linreg$new(Petal.Length~Species, data = iris)
#' linregtest$print()
#' linregtest$plot()
#' linregtest$summary()

linreg <-setRefClass( Class = "linreg",
                      fields= list(formula= "formula",
                                   data= "data.frame",
                                   dta= "character",
                                   frmla= "formula",
                                   X= "matrix",
                                   y= "matrix",
                                   Regressions_coefficients = "matrix",
                                   The_fitted_values = "matrix",
                                   The_residuals= "matrix",
                                   The_degrees_of_freedom= "numeric",
                                   The_residual_variance = "numeric",
                                   The_variance_of_the_regression_coefficients= "matrix",
                                   The_t_values= "matrix"

                      ),

                      methods=list(
                        initialize= function(formula,data){
                          X <<- model.matrix(formula, data)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          stopifnot (class(formula) == "formula")
                          stopifnot (class(data) == "data.frame")
                          frmla<<- formula
                          dta <<- deparse(substitute(data))



                          #Regressions coefficients:
                          Regressions_coefficients<<-solve(t(X) %*% X) %*% t(X) %*% y


                          #The fitted values:
                          The_fitted_values<<-X %*% Regressions_coefficients



                          #The residuals:
                          The_residuals<<- y -The_fitted_values


                          #The degrees of freedom:
                          The_degrees_of_freedom<<- nrow(X)-ncol(X)


                          #The residual variance:****(numeric or vector?)
                          The_residual_variance<<-as.numeric((t(The_residuals) %*% The_residuals) / The_degrees_of_freedom)


                          #The variance of the regression coefficients:
                          The_variance_of_the_regression_coefficients<<-  The_residual_variance * solve(t(X) %*% X)

                          #t-values for each coefficient:
                          The_t_values <<- Regressions_coefficients / sqrt(diag(The_variance_of_the_regression_coefficients))

                          #P-values
                          #p_values <<- 2 * pt(abs(The_t_values), The_degrees_of_freedom, lower.tail = FALSE)




                        },
                        print = function() {

                          cat("\n","Call:","\n",
                              paste("linreg(", "formula = ", frmla[2]," ", frmla[1], " ", frmla[3],", ", "data = ", dta, ")",sep = "", collapse = "\n" ),
                              "\n","Coefficients:","\n",
                              paste(row.names(Regressions_coefficients),
                                    sep = "  ", collapse ="  " ),"\n",
                              format(round(Regressions_coefficients,2), justify = "centre",width = 13))
                        },


                        resid = function(){
                          return(as.vector(The_residuals))
                        },
                        pred = function(){
                          return(The_fitted_values)
                        },
                        coef = function(){
                          a <- as.vector(Regressions_coefficients)
                          names(a) <- colnames(X)
                          return(a)
                        },

                        plot=function(){
                          library(ggplot2)
                          library(gridExtra)




                          p1<-ggplot()+
                            #geom_line(data=data,aes(x=all.vars(formula)[2],y=all.vars(formula)[1]),color="red")+
                            geom_point(data=data,aes(x=The_fitted_values,y=The_residuals),shape=1)+
                            ylab("Residuals")+xlab("Fitted values")+
                            labs(title = paste("Residuals vs Fitted values"))+
                            theme(plot.title = element_text(hjust = 0.5))

                          p2<-ggplot()+
                            #geom_line(data=data,aes(x=,y=),color="red")+
                            geom_point(data=data,aes(x=The_fitted_values,y=sqrt(abs(scale(The_residuals)))),shape=1)+
                            ylab(expression(sqrt(abs("Standardized residual"))))+xlab("Fitted values")+
                            labs(title = paste("Scale-Location"))+
                            theme(plot.title = element_text(hjust = 0.5))



                          return(grid.arrange(p1, p2, ncol = 1))
                        },

                        summary = function () {
                          cat("\nCall:\n")
                          cat(paste("linreg(formula = ", (format(frmla)), ", data = ", dta, ")\n\n", sep = ""))
                          cat("Coefficients:\n\n")

                          table = data.frame(matrix(ncol = 5, nrow = 0))
                          for (i in 1:length(Regressions_coefficients)){

                            t = Regressions_coefficients[i]/sqrt(The_variance_of_the_regression_coefficients[i, i])

                            p = 2 * pt(abs(t), The_degrees_of_freedom, lower.tail = FALSE)

                            newdataframe = data.frame(round(Regressions_coefficients[i], 2), round(sqrt(The_variance_of_the_regression_coefficients[i, i]), 2), round(t, 2), formatC(p, format = "e", digits = 2),"***")
                            rownames(newdataframe)[1] = rownames(The_variance_of_the_regression_coefficients)[i]
                            table = rbind(table, newdataframe)
                          }

                          colnames(table) = c(" ", "Estimate", "Standard Error", "t-value", "P-Value")
                          write.table(table, quote=FALSE)
                          cat(paste("\nResidual standard error:", sqrt(The_residual_variance), "on", The_degrees_of_freedom, "degrees of freedom"))

                        }
                      )
)
