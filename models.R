# Grafico predito vs observado -------------------------------------------------
gg_pred_obs<-function(data,medidas){
  ggplot(data,aes(x = actual_y,y = predicted_y)) +
    geom_point(alpha = 0.6) +
    geom_point(aes(x = actual_y,y = destaque1),alpha = 0.6,color = "red2")+
    geom_point(aes(x = actual_y,y = destaque2),alpha = 0.6,color = "blue2")+
    geom_abline(slope = 1,intercept = 0,linetype = "dashed") +
    coord_fixed(xlim = c(2,11),ylim = c(2,11)) +
    theme_bw(base_size = 12) +
    theme(plot.title = ggplot2::element_blank()) +
    labs(x = "Estresse observado",y = "Estresse predito",
         subtitle = sprintf(
           "RMSE = %.3f | R² = %.3f",
           medidas$.estimate[
             medidas$.metric == "rmse"
           ],
           medidas$.estimate[
             medidas$.metric == "rsq"
           ]
         )
    )
}

# Divisão do banco -------------------------------------------------------------

# Divisão treino/teste
set.seed(123)
split <- rsample::initial_split(
  dados_modelo,
  prop = 0.8
)
train_data <- rsample::training(split)
test_data  <- rsample::testing(split)
p <- ncol(train_data) - 1

# Modelo de Regressão Linear ===================================================
ols_spec <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")
recipe <- recipes::recipe(Nv_estresse ~ .,data = train_data)
ols_workflow <- workflows::workflow() |>
  workflows::add_recipe(recipe) |>
  workflows::add_model(ols_spec)
set.seed(123)
cv_folds <- rsample::vfold_cv(train_data,v = 10)

## Paralelização
n_cores <- parallel::detectCores(logical = FALSE) - 1
if(n_cores < 1) n_cores <- 1
future::plan(future::multisession, workers = n_cores)
#

set.seed(123)
ols_cv_results <- tune::fit_resamples(
  ols_workflow,resamples = cv_folds,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq),
  control = tune::control_resamples(save_pred = TRUE)
)
final_ols_fit <- workflows::fit(ols_workflow,data = train_data)
# broom::tidy(final_ols_fit)[,-4]

# predito:
predictions_ols <- predict(final_ols_fit,new_data = test_data)
val_results_ols <- test_data |>
  dplyr::select(Nv_estresse) |>
  dplyr::bind_cols(predictions_ols) |>
  dplyr::rename(actual_y = Nv_estresse,predicted_y = .pred)
final_metrics <- yardstick::metric_set(yardstick::rmse,yardstick::rsq)
final_eval_ols <- val_results_ols |>
  final_metrics(truth = actual_y,estimate = predicted_y)
val_results_ols <- val_results_ols |>
  dplyr::mutate(
    destaque1 = dplyr::if_else(actual_y < 8 & predicted_y > actual_y, predicted_y, NA),
    destaque2 = dplyr::if_else(actual_y > 9 & predicted_y < actual_y, predicted_y, NA)
  )
G_OLS<-gg_pred_obs(data = val_results_ols,medidas=final_eval_ols)

# Árvore de Decisão ============================================================

dt_spec <- parsnip::decision_tree(
  cost_complexity = parsnip::tune(),
  tree_depth = parsnip::tune(),
  min_n = parsnip::tune()) |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("rpart")
dt_workflow <- workflows::workflow() |>
  workflows::add_recipe(recipe) |>
  workflows::add_model(dt_spec)
dt_grid <- dials::grid_regular(
  dials::cost_complexity(range = c(-4, -1)),
  dials::tree_depth(range = c(2L, 8L)),
  dials::min_n(range = c(2L, 30L)),
  levels = 4
)
set.seed(123)
dt_tune_results <- tune::tune_grid(
  dt_workflow,resamples = cv_folds,grid = dt_grid,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq)
)
best_params_dt <- tune::select_best(dt_tune_results,metric = "rmse")
# best_params_dt
final_dt_workflow <- tune::finalize_workflow(dt_workflow,best_params_dt)
final_dt_fit <- workflows::fit(final_dt_workflow,data = train_data)
final_rpart_model <- tune::extract_fit_engine(final_dt_fit)
#graficos da arvore e importancia
rpart.plot::rpart.plot(final_rpart_model,roundint = FALSE)
G_inf_DT<-final_dt_fit |>
  tune::extract_fit_parsnip() |>
  vip::vip()+
  geom_col(fill = c(rep("blue3",2),rep("gray30",3)), color = "black") +
  theme_bw(base_size = 12) +
  theme(plot.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.7, l = 0.0, unit = "cm"))
predictions_dt <- predict(final_dt_fit,new_data = test_data)
test_results_dt <- test_data |>
  dplyr::select(Nv_estresse) |>
  dplyr::bind_cols(predictions_dt) |>
  dplyr::rename(actual_y = Nv_estresse,predicted_y = .pred)
final_eval_dt <- test_results_dt |>
  yardstick::metric_set(yardstick::rmse, yardstick::rsq)(
    truth = actual_y,estimate = predicted_y)
test_results_dt <- test_results_dt |>
  dplyr::mutate(
    destaque1 = dplyr::if_else(actual_y < 8 & predicted_y > actual_y, predicted_y, NA),
    destaque2 = dplyr::if_else(actual_y > 9 & predicted_y < actual_y, predicted_y, NA)
  )
G_DT<-gg_pred_obs(test_results_dt,medidas = final_eval_dt)

# Random Forest ================================================================

rf_spec <- parsnip::rand_forest(mtry = parsnip::tune(),
  min_n = parsnip::tune(),trees = parsnip::tune()) |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("ranger",importance = "permutation")
rf_workflow <- workflows::workflow() |>
  workflows::add_recipe(recipe) |>
  workflows::add_model(rf_spec)
rf_grid <- dials::grid_regular(
  dials::mtry(range = c(1L, p)),
  dials::min_n(range = c(2L, 30L)),
  dials::trees(range = c(100, 500L)),
  levels = c(
    mtry = p,
    min_n = 5,
    trees = 5
  )
)
set.seed(123)
rf_tune_results <- tune::tune_grid(rf_workflow,
  resamples = cv_folds,grid = rf_grid,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq))
autoplot(rf_tune_results) +
  theme_minimal() +
  labs(
    title = "Random Forest Cross-Validation Performance"
  )
best_params_rf <- tune::select_best(rf_tune_results,metric = "rmse")
final_rf_workflow <- tune::finalize_workflow(rf_workflow,best_params_rf)
set.seed(123)
final_rf_fit <- workflows::fit(final_rf_workflow,data = train_data)
G_inf_RF<-final_rf_fit |> tune::extract_fit_parsnip() |>
  vip::vip() +
  geom_col(fill = c(rep("blue3",2),rep("gray30",4)), color = "black") + theme_bw(base_size = 12) +
  theme(plot.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.margin = margin(t = 0.2, r = 0.0, b = 0.7, l = 0.0, unit = "cm"))
predictions_rf <- predict(final_rf_fit,new_data = test_data)
test_results_rf <- test_data |>
  dplyr::select(Nv_estresse) |>
  dplyr::bind_cols(predictions_rf) |>
  dplyr::rename(actual_y = Nv_estresse,predicted_y = .pred)
final_eval_rf <- test_results_rf |> final_metrics(
  truth = actual_y,estimate = predicted_y)
test_results_rf <- test_results_rf |>
  dplyr::mutate(
    destaque1 = dplyr::if_else(actual_y < 8 & predicted_y > actual_y, predicted_y, NA),
    destaque2 = dplyr::if_else(actual_y > 9 & predicted_y < actual_y, predicted_y, NA)
  )
G_RF<-gg_pred_obs(data = test_results_rf,medidas = final_eval_rf)

# Salvando informações ---------------------------------------------------------

save(G_OLS,G_DT,G_RF,file = "inf/G_prev.RData")

tabela_ols <- broom::tidy(final_ols_fit) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(p_formatado = dplyr::if_else(p.value < 0.001, "(p < 0.001)",
                                             paste0("(p = ", round(p.value, 3),")")))
G_inf_OLS <- ggplot(tabela_ols, aes(x = reorder(term, abs(estimate)), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.2,alpha = 0.5) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = c(rep("black",2),rep("blue3",2),rep("black",2)),
                size = 0.6) +
  geom_point(color = c(rep("black",2),rep("blue3",2),rep("black",2)), size = 1.5) +
  geom_text(aes(label = p_formatado), vjust = -1.0, size = 2.5, color = "gray20") +
  coord_flip() +
  expand_limits(y = c(min(tabela_ols$estimate) - 0.2, max(tabela_ols$estimate) + 0.2)) +
  labs(y = "Estimativa com IC 95%") +
  theme_bw(base_size = 12) +
  theme(plot.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.7, l = 0.0, unit = "cm"))

save(G_inf_OLS,G_inf_DT,G_inf_RF,file = "inf/G_inf.RData")



