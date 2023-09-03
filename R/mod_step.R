mod_step = function(dataset, responsecol, direction = "stepwise"){

  colnames(dataset) = chartr(" ", "_", colnames(dataset))

  # to add or remove a variable from the existing model
  re_model = function(base_list, res){
    return(reformulate(base_list, response = res))
  }

  # forward selection
  forward = function(dataset, response = responsecol,  base_var_lst = c("1")){

    base_eq = reformulate(base_var_lst, response)
    unused_var = setdiff(colnames(dataset), base_var_lst)
    unused_var = unused_var[unused_var != response]

    Pval_df = data.frame(Added_Variable = "none", F = 0, P_value = 0)

    for (i in 1:length(unused_var)){
      cl = unused_var[i]
      test = anova(lm(base_eq, data = dataset), lm(re_model(c(base_var_lst, cl), response), data = dataset))
      Pval_df = rbind(Pval_df, list(cl, test$F[[2]], test$'Pr(>F)'[[2]]))
    }
    Pval_df = Pval_df[-1,]
    print(Pval_df)
    p_min = min(Pval_df$P_value)
    f_max = max(Pval_df$F[Pval_df$P_value == p_min])
    if (!is.na(p_min) && p_min <= 0.05){
      dup = Pval_df$P_value[duplicated(Pval_df$P_value)]
      if (p_min %in% dup){
        add_var = Pval_df$Added_Variable[Pval_df$F == f_max]
      }else{
        add_var = Pval_df$Added_Variable[Pval_df$P_value == p_min]
      }
      cat("\n", "Addded Variable is:", add_var, "\n")
      return(c(base_var_lst, add_var))
    }else{
      cat("\n", "!Variables can't be added further!", "\n")
      return(base_var_lst)
    }

  }

  # backward elemenation
  backward = function(dataset, response = responsecol, base_var_lst = colnames(dataset)[colnames(dataset) != response]){

    base_eq = reformulate(base_var_lst, response)
    Pval_df = data.frame(Removed_Variable = "none", F = 0, P_value = 0)

    for (i in 1:length(base_var_lst)){
      cl = base_var_lst[i]
      test = anova(lm(re_model(base_var_lst[base_var_lst != cl], response), data = dataset), lm(base_eq, data = dataset))
      Pval_df = rbind(Pval_df, list(cl, test$F[[2]], test$'Pr(>F)'[[2]]))
    }
    Pval_df = Pval_df[-1,]
    print(Pval_df)
    p_max = max(Pval_df$P_value)
    f_min = min(Pval_df$F[Pval_df$P_value == p_max])
    if (!is.na(p_max) && p_max > 0.05){
      dup = Pval_df$P_value[duplicated(Pval_df$P_value)]
      if (p_max %in% dup){
        rem_var = Pval_df$Removed_Variable[Pval_df$F == f_min]
      }else{
        rem_var = Pval_df$Removed_Variable[Pval_df$P_value == p_max]
      }
      cat("\n", "Removed Variable is:", rem_var, "\n")
      return(base_var_lst[base_var_lst != rem_var])
    }else{
      cat("\n", "!Variables can't be removed further!", "\n")
      return(base_var_lst)
    }

  }

  # if forward method is selected
  if (direction == "forward"){

    cat("\n", "           ->Forward Selection<-           ", "\n\n")
    cat("\n", "++ Step 1 ++", "\n")
    final_vars = forward(dataset)
    for (i in 1:length(dataset)-2){
      cat("\n", "++ Step", i+3, "++", "\n")
      updated_vars = forward(dataset, base_var_lst = final_vars)
      if (!setequal(final_vars, updated_vars)){
        final_vars = updated_vars
      }
      else{
        break
      }
    }
    cat("\n", "**********FINAL MODEL**********", "\n")
    cat("\n","Rejected variables from the model:", setdiff(colnames(dataset), c(final_vars, responsecol)), "\n")
    final_model = reformulate(final_vars, responsecol)
    return(summary(lm(final_model, data = dataset)))

    # if backward method is selected
  }else if (direction == "backward"){

    cat("\n", "           ->Backward Elemenation<-           ", "\n\n")
    cat("\n", "++ Step 1 ++", "\n")
    final_vars = backward(dataset)
    for (j in 1:length(dataset)-2){
      cat("\n", "++ Step", j+3, "++", "\n")
      updated_vars = backward(dataset, base_var_lst = final_vars)
      if (!setequal(final_vars, updated_vars)){
        final_vars = updated_vars
      }
      else{
        break
      }
    }
    cat("\n", "**********FINAL MODEL**********", "\n")
    cat("\n","Rejected variables from the model:", setdiff(colnames(dataset), c(final_vars, responsecol)), "\n")
    final_model = reformulate(final_vars, responsecol)
    return(summary(lm(final_model, data = dataset)))

    # if stepwise method is selected
  }else if (direction == "stepwise"){

    cat("\n", "           ->Stepwise<-           ", "\n\n")
    cat("\n", "++ Step 1 ++", "\n")
    cat("\n", "--Adding Step--","\n")
    forward_vars = forward(dataset)
    cat("\n", "--Removing Step--","\n")
    backward_vars = backward(dataset, base_var_lst = forward_vars)
    final_vars = character(0)
    k = 2;
    while (!setequal(final_vars, backward_vars)){
      final_vars = backward_vars
      cat("\n", "---------------------------------------", "\n")
      cat("\n", "++ Step", k, "++", "\n")
      cat("\n", "--Adding Step--","\n")
      forward_vars = forward(dataset, base_var_lst = final_vars)
      cat("\n", "--Removing Step--","\n")
      backward_vars = backward(dataset, base_var_lst = forward_vars)
      k = k + 1
    }
    cat("\n", "**********FINAL MODEL**********", "\n")
    cat("\n","Rejected variables from the model:", setdiff(colnames(dataset), c(final_vars, responsecol)), "\n")
    final_model = reformulate(final_vars, responsecol)
    return(summary(lm(final_model, data = dataset)))

  }else{
    return("Please check your input arguments!!")
  }
}
