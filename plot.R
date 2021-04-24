library(ggplot2)
library(plotly)

l = list.files(path = "task_1", pattern = ".txt", full.names = TRUE);

for (filename in list.files(path = "task_1", pattern = ".txt", full.names = TRUE)) {
  table = read.table(filename)
  
  str = substr(filename,
               start = sapply(gregexpr("/", filename), '[[', 1) + 1,
               stop = sapply(gregexpr("\\.", filename), '[[', 1) - 1
               );
  
  show(str);
  
  l = strsplit(str, split = " ")
  
  first = sapply(l, '[[', 1);
  
  if (first == "n_evaluations_by_eps") {
    png(paste("task_1/", str, ".png"));
    plot(x = table[[1]], y = table[[2]], pch = 1, log="x", type = "o", main = "Количество вычислений от eps",
         xlab = "eps", ylab = "number of evaluations", col = "green",
         ylim=c(min(table[[2]], table[[3]], table[[4]]), max(table[[2]], table[[3]], table[[4]]) )) %>%
      lines(x = table[[1]], y = table[[3]], pch = 1, type = "o", col = "blue") %>%
      lines(x = table[[1]], y = table[[4]], pch = 1, type = "o", col = "red")
      legend("topright", c(sapply(l, '[[', 2),
                           sapply(l, '[[', 3),
                           sapply(l, '[[', 4)
                           ), col=c("green", "blue", "red"), pch=c(1, 1, 1));
      dev.off();
  } else if (first == "n_steps_by_eps") {
    png(paste("task_1/", str, ".png"));
    
    plot(x = table[[1]], y = table[[2]], pch = 1, log="x", type = "o", main = "Количество шагов от eps",
         xlab = "eps", ylab = "number of steps", col = "green",
         ylim=c(min(table[[2]], table[[3]], table[[4]]), max(table[[2]], table[[3]], table[[4]]) )) %>%
      lines(x = table[[1]], y = table[[3]], pch = 1, type = "o", col = "blue") %>%
      lines(x = table[[1]], y = table[[4]], pch = 1, type = "o", col = "red")
    legend("topright", c(sapply(l, '[[', 2),
                         sapply(l, '[[', 3),
                         sapply(l, '[[', 4)
    ), col=c("green", "blue", "red"), pch=c(1, 1, 1));
    dev.off();
  } else if (first == "interval_length") {
    png(paste("task_1/", str, ".png"));
    
    plot(x = table[[1]], y = table[[2]], pch = 1, log = "y", type = "o",
         main = paste("Длинна поискового отрезка \n для метода ", c(sapply(l, '[[', 2)),
                      ", \nфункции ", c(sapply(l, '[[', 3)),
                      " с eps = ", c(sapply(l, '[[', 4))),
         xlab = "number of steps", ylab = "delta", col = "green",
         ylim = c(min(table[[2]]), max(table[[2]])))
    legend("topright", c(sapply(l, '[[', 2)
    ), col=c("green"), pch=c(1));
    dev.off();
  } else {
    #show(table);
    
    png(paste("task_1/", str, ".png"));
    
    plot(x = table[[1]], y = table[[2]], pch = 1, type = "o",
         main = paste("Поисковый отрезок \n для метода ", c(sapply(l, '[[', 1)),
                      ", \n функции ", c(sapply(l, '[[', 2)),
                      " с eps = ", c(sapply(l, '[[', 3))),
         xlab = "number of steps", ylab = "delta", col = "green",
         ylim=c(min(table[[2]], table[[3]]), max(table[[2]], table[[3]]) )) %>%
    lines(x = table[[1]], y = table[[3]], pch = 1, type = "o", col = "blue")
    legend("topright", c("a", "b"), col=c("green", "blue"), pch=c(1, 1));
    dev.off();
  }
}


m = matrix(nrow = 2, ncol = 2);
m[1, 1] = 0;
m[1, 2] = 2;
m[2, 1] = 2;
m[2, 2] = 0;


eigen(m);


generate_random_matrix = function(n, o) {
  m = matrix(nrow = n, ncol = n);
  for (i in 1 : n) {
    m[i, i] = 1 + (o * 0.9 - 1) / (n - 1) * (i - 1);
  }
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      m[i, j] = m[j, i] = runif(1, 0, o * 0.9);
    }
  }
  return(m);
}

calc_conditionality_number = function(m) {
  e = eigen(m);
  max_val = max(e$values);
  min_val = min(e$values);
  
  return(max_val / min_val);
}


modify_main_components = function(m, c) {
  for (i in 1 : dim(m)[1]) {
    m[i, i] = m[i, i] * c;
  }
  return(m)
};

generate_matrix = function(n, o) {
  m = generate_random_matrix(n, o);
  l = 1;
  r = n * o * 100;
  
  
  
  show(modify_main_components(m, r));
  show(modify_main_components(m, l));
  
  show(calc_conditionality_number(modify_main_components(m, r)));
  show(calc_conditionality_number(modify_main_components(m, l)));
  
  return(m)
}

m = generate_matrix(3, 7);
m;

modify_main_components(m, 5);


dim(m);

m = generate_random_matrix(3, 2);
m;
e = eigen(m);
e

calc_conditionality_number(m);

e$values


min_x = -1;
max_x = 2;

min_y = -1;
max_y = 2;

resolution = 40;

f = function(x, y) {
  return(x * x + 10 * y * y + x * y);
}

for (i in 0 : 3) {

  table = read.table(paste0("task_2/x^2 + 10y^2 + xy ", i, ".txt"))
  
  
  drow_func = function(min_x, max_x, min_y, max_y, resolution, f) {
    x_values = seq(min_x, max_x, length.out = resolution);
    y_values = seq(min_y, max_y, length.out = resolution);
    
    
    f_values = matrix(nrow = resolution, ncol = resolution);
    
    for (i in 1:resolution) {
      for (j in 1:resolution) {
        f_values[j, i] = f(x_values[i], y_values[j]);
      }
    }
    
    p = filled.contour(x = x_values, y = y_values, z = f_values,
                       plot.axes={axis(1); axis(2); lines(x = table[[1]], y = table[[2]], type='l');},
                       main = "x^2 + 10y^2 + xy")
    return (p);
  };
  
  png(paste0("task_2/x^2 + 10y^2 + xy", i, ".png"));
  
  drow_func(min_x, max_x, min_y, max_y, resolution, f);
  dev.off();
}


drow_func = function(min_x, max_x, min_y, max_y, resolution, f) {
  x_values = seq(min_x, max_x, length.out = resolution);
  y_values = seq(min_y, max_y, length.out = resolution);
  
  
  f_values = matrix(nrow = resolution, ncol = resolution);
  
  for (i in 1:resolution) {
    for (j in 1:resolution) {
      f_values[j, i] = f(x_values[i], y_values[j]);
    }
  }
  
  p = plot_ly(x = x_values, y = y_values, z = f_values)%>%
    add_surface(contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
    );


  p = add_paths(p,
                x = c(-1, -0.9801980204, -0.9801980198, -0.9605920993, -0.9605920988, -0.9411802962, -0.9411802958, -0.9219606891, -0.9219606889, -0.9029313752, -0.9029313752, -0.8840904698, -0.8840904705, -0.8654361079, -0.8654361094, -0.8469664437, -0.846966445, -0.8286796474, -0.8286796485, -0.8105739092, -0.8105739095, -0.7926474357, -0.7926474351, -0.774898451, -0.7748984506, -0.7573251989, -0.7573251986, -0.7399259394, -0.7399259392, -0.7226989498, -0.7226989497, -0.7056425238, -0.7056425245, -0.6887549733, -0.6887549747, -0.6720346273, -0.6720346285, -0.6554798292, -0.6554798302, -0.6390889405, -0.6390889408, -0.6228603379, -0.6228603374, -0.6067924137, -0.6067924133, -0.5908835778, -0.5908835775, -0.5751322551, -0.575132255, -0.5595368861, -0.5595368861, -0.5440959262, -0.5440959268, -0.5288078471, -0.5288078484, -0.5136711359, -0.513671137, -0.4986842932, -0.4986842941, -0.4838458355, -0.4838458358, -0.4691542933, -0.4691542928, -0.4546082111, -0.4546082107, -0.4402061495, -0.4402061492, -0.4259466825, -0.4259466824, -0.4118283984, -0.4118283984, -0.3978498989, -0.3978498994, -0.3840098003, -0.3840098014, -0.3703067331, -0.3703067341, -0.3567393399, -0.3567393407, -0.3433062777, -0.3433062779, -0.3300062162, -0.3300062158, -0.3168378377, -0.3168378374, -0.3037998392, -0.303799839, -0.2908909298, -0.2908909297, -0.2781098314, -0.2781098314, -0.2654552781, -0.2654552786, -0.2529260174, -0.2529260184, -0.2405208095, -0.2405208104, -0.2282384254, -0.2282384261, -0.2160776494, -0.2160776496)
, y = c(-1, -1, -0.9801980197, -0.9801980198, -0.9605920985, -0.9605920988, -0.9411802954, -0.9411802958, -0.9219606884, -0.9219606889, -0.9029313756, -0.9029313752, -0.8840904717, -0.8840904705, -0.8654361108, -0.8654361094, -0.8469664466, -0.846966445, -0.8286796493, -0.8286796486, -0.8105739095, -0.8105739095, -0.792647435, -0.7926474351, -0.7748984503, -0.7748984506, -0.7573251982, -0.7573251986, -0.7399259387, -0.7399259392, -0.72269895, -0.7226989497, -0.7056425256, -0.7056425245, -0.688754976, -0.6887549748, -0.6720346299, -0.6720346285, -0.6554798309, -0.6554798302, -0.6390889408, -0.6390889408, -0.6228603373, -0.6228603374, -0.6067924131, -0.6067924133, -0.5908835772, -0.5908835775, -0.5751322545, -0.575132255, -0.5595368864, -0.5595368861, -0.5440959278, -0.5440959268, -0.5288078495, -0.5288078484, -0.5136711383, -0.513671137, -0.4986842947, -0.4986842941, -0.4838458358, -0.4838458358, -0.4691542927, -0.4691542928, -0.4546082105, -0.4546082107, -0.4402061489, -0.4402061492, -0.425946682, -0.4259466824, -0.4118283987, -0.4118283984, -0.3978499003, -0.3978498994, -0.3840098025, -0.3840098014, -0.3703067353, -0.3703067341, -0.3567393413, -0.3567393407, -0.3433062779, -0.3433062779, -0.3300062157, -0.3300062158, -0.3168378372, -0.3168378374, -0.3037998387, -0.303799839, -0.2908909294, -0.2908909297, -0.2781098316, -0.2781098314, -0.2654552794, -0.2654552786, -0.2529260194, -0.2529260185, -0.2405208114, -0.2405208104, -0.2282384266, -0.2282384261, -0.2160776496)
, z = c(4.04, 4, 3.96039604, 3.921184198, 3.882360592, 3.843921378, 3.80586275, 3.768180941, 3.730872219, 3.69393289, 3.657359297, 3.621147819, 3.58529487, 3.549796901, 3.514650397, 3.479851878, 3.445397899, 3.411285049, 3.37750995, 3.344069257, 3.31095966, 3.278177882, 3.245720675, 3.213584827, 3.181767155, 3.15026451, 3.119073772, 3.088191854, 3.057615697, 3.027342274, 2.997368588, 2.967691671, 2.938308585, 2.909216421, 2.880412298, 2.851893365, 2.823656797, 2.795699799, 2.768019603, 2.740613468, 2.713478681, 2.686612556, 2.660012432, 2.633675675, 2.607599678, 2.581781859, 2.556219663, 2.530910557, 2.505852037, 2.481041621, 2.456476852, 2.432155299, 2.408074554, 2.384232231, 2.360625972, 2.337253437, 2.314112314, 2.291200311, 2.26851516, 2.246054613, 2.223816449, 2.201798464, 2.179998479, 2.158414336, 2.137043897, 2.115885047, 2.09493569, 2.074193752, 2.05365718, 2.033323941, 2.013192021, 1.993259427, 1.973524185, 1.953984341, 1.934637962, 1.91548313, 1.896517951, 1.877740546, 1.859149055, 1.840741639, 1.822516474, 1.804471756, 1.786605699, 1.768916534, 1.751402509, 1.73406189, 1.71689296, 1.69989402, 1.683063386, 1.666399392, 1.649900388, 1.633564741, 1.617390833, 1.601377062, 1.585521844, 1.569823608, 1.5542808, 1.538891881, 1.523655328, 1.508569631, 1.493633298)
,

                color = I("red"),  name = "1");
  
  
  p = add_paths(p,
                x = c(-1, -0.9801980204, 1.000000001, 1.000000001, 1)
                ,
                y = c(-1, -1, 1.000000001, 1.000000001, 1)
              ,
                z = c(4.04, 4, 3.811061522e-017, 1.004087808e-018, 2.779304496e-025)
                , color = I("green"),  name = "2");
  
 
  
  return (p);
};

drow_func(min_x, max_x, min_y, max_y, resolution, f);




y = c(3, 2.501221825, 0.9999975031, 0.999999983, 0.9999999999, 0.9999999999);


y = c(3, 2.501221825, 0.9999975031, 0.999999983, 0.9999999999, 0.9999999999);


min_x = 0;
max_x = 4;

min_y = 0;
max_y = 4;

f = function(x, y) {
  return(-2 * exp(-((x - 2) / 2) ^ 2 - ((y - 1) / 1) ^2)
         -3 * exp(-((x - 2) / 3) ^ 2 - ((y - 3) / 2) ^2)
         );
};

drow_func = function(min_x, max_x, min_y, max_y, resolution, f) {
  x_values = seq(min_x, max_x, length.out = resolution);
  y_values = seq(min_y, max_y, length.out = resolution);
  
  
  f_values = matrix(nrow = resolution, ncol = resolution);
  
  for (i in 1:resolution) {
    for (j in 1:resolution) {
      f_values[j, i] = f(x_values[i], y_values[j]);
    }
  }
  
  p = plot_ly(x = x_values, y = y_values, z = f_values)%>%
    add_surface(contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
    );
  
  
  p = add_paths(p,
                x = c(2, 2, 2)
                , y = c(2, 2.221819872, 2.889024332)
                , z = c(-2.880317824, -2.899292636, -3.004355737)
                ,
                color = I("red"),  name = "1");
  
  p = add_paths(p,
                x = c(2, 2, 2)
                , y = c(2, 2.221819872, 2.889024332)
                , z = c(-2.880317824, -2.899292636, -3.004355737)
                ,
                color = I("green"),  name = "2");
  
  p = add_paths(p,
                x = c(2, 2, 2, 2, 2)
                , y = c(2, 1.960524554, 1.959054412, 1.959051725, 1.959051725)
                , z = c(-2.880317824, -2.879874798, -2.879874241, -2.879874241, -2.879874241)
                ,
                color = I("blue"),  name = "3");
  
  return (p);
};


drow_func(min_x, max_x, min_y, max_y, resolution, f);









require("grDevices") # for colours
filled.contour(volcano, asp = 1) # simple

x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano,
               color.palette = function(n) hcl.colors(n, "terrain"),
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                 axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"),
               key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

# Annotating a filled contour plot
a <- expand.grid(1:20, 1:20)
b <- matrix(a[,1] + a[,2], 20)
filled.contour(x = 1:20, y = 1:20, z = b,
               plot.axes = { axis(1); axis(2); points(10, 10) })

## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, length.out = 27)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
## rather, the key *should* be labeled:
filled.contour(cos(r^2)*exp(-r/(2*pi)), frame.plot = FALSE,
               plot.axes = {})

