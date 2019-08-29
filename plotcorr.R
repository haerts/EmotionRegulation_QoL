plotcorr <- function(dv,iv,post_results) {

names(post_results) <- sub(dv, 'dv', names(post_results))
names(post_results) <- sub(iv, 'iv', names(post_results))

m <- lm(dv ~ iv, data = post_results)

plot <-plot_ly(data= post_results[post_results$group == 'CON',] , x=~iv , name = 'Caregivers',color=I("blue"))%>%
  add_markers(y = ~dv) %>%
  add_trace(data = post_results[post_results$group == 'MEN',],x=~iv,y=~dv, type = 'scatter',mode = 'markers',
            color=I("cyan3"), name = 'Meningioma patients')%>%
  add_trace(data = post_results[post_results$group == 'GLI',],x=~iv,y=~dv, type = 'scatter',mode = 'markers',
            color=I("magenta"), name = 'Glioma patients')%>%
  add_lines(y = fitted(m), data = post_results, x=~iv,name = 'Regression line',
            line = list(color = 'rgba(100, 100, 100, 1)')) %>%
  add_ribbons(data =augment(m),
              ymin = ~.fitted - 1.96 * .se.fit,
              ymax = ~.fitted + 1.96 * .se.fit,
              line = list(color = 'rgba(110, 110, 110, 0.2)'),
              fillcolor = 'rgba(110,110,110, 0.3)',
              name = "95% confidence interval") %>%
  layout(xaxis = list(title = iv),
         yaxis = list(title = dv),
         legend = list(x = 0.80, y = 0.90))

return(plot)
}
