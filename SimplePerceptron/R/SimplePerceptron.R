#' @title Simple Perceptron
#'
#' @description It performs simple perceptron on data
#'
#' @param df
#' @param clr
#' @param design
#'
#' @return NULL
#'
#' @examples
#'
#' @export

SimplePerceptron<-function(df,clr,design)
{
  add_boundary <- function(w, clr){

    w <- as.numeric(w)

    b = w[1]

    w1 = w[2]

    w2 = w[3]



    slope = -(w1/w2)

    intercept = -(b/w2)

    abline(a=intercept, b=slope, col=clr)

  }



  activate <- function(df,w){

    activation <- apply(df, 1, function(x) sum(x*w))

    return(ifelse(activation > 0, 1, -1))

  }



  random_row <- function(mis_classifs){

    rand_choice <- if(nrow(mis_classifs)==1){

      as.numeric(rownames(mis_classifs))

    }else{

      sample(as.numeric(rownames(mis_classifs)),1)

    }

    return(rand_choice)

  }


  w_true <- runif(3, -5, 5)

  f_class <- activate(df, w_true)



  #plot data

  plot(df[,2],df[,3], col=(factor(f_class)),pch=design)

  add_boundary(w_true,clr)



  #LEARNING

  w_guess <- runif(3, -.5, .5)


  #guess weights

  g_class <- activate(df, w_guess)



  #Perceptron Learning Algorithm



  while(!all(g_class==f_class)){ #mis-classifications



    #randomly choose one mis-classified example

    mis_classifs <- df[g_class != f_class,]

    rand_choice <- random_row(mis_classifs)



    #update the weights

    w_guess <- w_guess + f_class[rand_choice]*as.vector(mis_classifs[rownames(mis_classifs)==rand_choice,])


    g_class <- activate(df, w_guess)


  }


  add_boundary(w_guess, clr) #plot final boundary
}

