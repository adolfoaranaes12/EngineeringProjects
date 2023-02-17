%% Reto %%

clc
clear all
close all

%% Importar datos de Excel %%

datatable = readtable('medicionesmatlab.xls','Range','A1:P36');
data = table2array(datatable);

%% Vectores de datos %%

% Mercurio -------------
dataM1 = data(:,1);
dataM1E = data(:,2);
dataM2 = data(:,3);
dataM2E = data(:,4);
dataM3 = data(:,5);
dataM3E = data(:,6);
dataM4 = data(:,7);
dataM4E = data(:,8);

dataM = [dataM1, dataM2, dataM3, dataM4];
dataME = [dataM1E, dataM2E, dataM3E, dataM4E];

% ARDUINO
dataA1 = data(:,9);
dataA1E = data(:,10);
dataA2 = data(:,11);
dataA2E = data(:,12);
dataA3 = data(:,13);
dataA3E = data(:,14);
dataA4 = data(:,15);
dataA4E = data(:,16);

dataA = [dataA1, dataA2, dataA3, dataA4];
dataAE = [dataA1E, dataA2E, dataA3E, dataA4E];

t = 1:30:1080;

%% Técnicas estadísticas %%

% Promedio (Mercurio y ARDUINO)
pM = mean(dataM);
pA = mean(dataA);

% Varianza (Mercurio y ARDUINO)
s2M = var(dataM,0,1);
s2A = var(dataA,0,1);

% Desviación estándar (Mercurio y ARDUINO)
sM = sqrt(s2M);
sA = sqrt(s2A);

% Coeficiente porcentual de variación de la media (Mercurio y ARDUINO)
cM = (sM./abs(pM))*100;
cA = (sA./abs(pA))*100;

%% Errores e incertidumbre %%

% Error estándar de la media (Mercurio y ARDUINO)
seM = sM./sqrt(length(dataM(:,1)));
seA = sA./sqrt(length(dataA(:,1)));

% Incertidumbre del Termómetro de mercurio
k = 1.96;
tol = 0.5; % Tolerancia
res = 1; % Resolución
tmin = -20; % Temperatura mínima
tmax = 130; % Temperatura máxima
uaM = seM; % Incertidumbre tipo A
ub1M = (res/(abs(tmin)+abs(tmax)))/sqrt(3); % Incertidumbre tipo B1
ub2M = tol/(sqrt(3)); % Incertidumbre tipo B2
ucM = sqrt(uaM.^2 + ub1M.^2 + ub2M.^2); % Incertidumbre estándar combinada
UM = k*ucM; % Incertidumbre expandida

% Incertidumbre del termoresistor con el ARDUINO
k = 1.96;
tol = 0.4; % Tolerancia
res = 1; % Resolución
tmin = -55; % Temperatura mínima
tmax = 150; % Temperatura máxima
uaA = seA; % Incertidumbre tipo A
ub1A = (res/(abs(tmin)+abs(tmax)))/sqrt(3); % Incertidumbre tipo B1
ub2A = tol/(2*sqrt(3)); % Incertidumbre tipo B2
ucA = sqrt(uaA.^2 + ub1A.^2 + ub2A.^2); % Incertidumbre estándar combinada
UA = k*ucA; % Incertidumbre expandida


%% Generación de tablas
VarNames = ["Instrumentos", "Promedio", "Varianza", "Desviación Estándar", ...
    "Coeficiente % de variación de la media", "Error estándar de la media"...
    "Incertidumbre Expandida"];
Instrumentos = ["Termómetro Mercurio";"LM35"];
P = [mean(pM); mean(pA)];
V = [mean(s2M); mean(s2A)];
DE = [mean(sM); mean(sA)];
C = [mean(cM); mean(cA)];
EE = [mean(seM); mean(seA)];
In = [mean(UM); mean(UA)];
table(Instrumentos, P, V, DE, C, EE, In, 'VariableNames',VarNames)

%% Comparación entre gráficas

color = {'#0072BD','#D95319','#EDB120','#7E2F8E','#77AC30','#4DBEEE','#A2142F','#000000'};
df1 = [0,1080,2*1080,3*1080];
df2 = [0,1080,0,1080];

figure('Name','Mediciones de temperatura')

subplot(3,2,[1 2])
    for itt = 1:4; scatter(t+df1(itt),dataM(:,itt),'filled','d','MarkerFaceColor',color{itt}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataM(:,itt),'Color',color{itt}); hold on; end
    grid on; axis padded; title('Mediciones de temperatura con termometro de Mercurio','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

subplot(3,2,[3 4])
    for itt = 1:4; scatter(t+df1(itt),dataA(:,itt),'filled','d','MarkerFaceColor',color{itt+4}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataA(:,itt),'Color',color{itt+4}); hold on; end
    grid on; axis padded; title('Mediciones de temperatura con termo-resistor','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

subplot(3,2,[5 6])
    for itt = 1:4; scatter(t+df1(itt),dataM(:,itt),'filled','d','MarkerFaceColor',color{itt}); hold on; end
    hold on
    for itt = 1:4; scatter(t+df1(itt),dataA(:,itt),'filled','d','MarkerFaceColor',color{itt+4}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataM(:,itt),'Color',color{itt}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataA(:,itt),'Color',color{itt+4}); hold on; end
    grid on; axis padded; title("Comparaci\'ones de Mediciones de temperatura",'FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1 Hg","Medici\'on 2 Hg","Medici\'on 3 Hg","Medici\'on 4 Hg",...
        "Medici\'on 1 Tr","Medici\'on 2 Tr","Medici\'on 3 Tr","Medici\'on 4 Tr",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

%% Comparación en histeresis

figure('Name','Histeresis')

subplot(2,2,1)
    scatter(t,dataM(:,1),'filled','d','MarkerFaceColor',color{1}); hold on
    scatter(t,flip(dataM(:,2)),'filled','d','MarkerFaceColor',color{2}); hold on
    plot(t,dataM(:,1),'Color',color{1}); hold on; 
    plot(t,flip(dataM(:,2)),'Color',color{2})
    grid on; axis padded; title('Histeresis en termometro de Mercurio: Experimento 1','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2",'location','southeast','Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

subplot(2,2,2)
    scatter(t,dataM(:,3),'filled','d','MarkerFaceColor',color{3}); hold on
    scatter(t,flip(dataM(:,4)),'filled','d','MarkerFaceColor',color{4}); hold on
    plot(t,dataM(:,3),'Color',color{3}); hold on; 
    plot(t,flip(dataM(:,4)),'Color',color{4})
    grid on; axis padded; title('Histeresis en termometro de Mercurio: Experimento 2','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 3","Medici\'on 4",'location','southeast','Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;


subplot(2,2,3)
    scatter(t,dataA(:,1),'filled','d','MarkerFaceColor',color{5}); hold on
    scatter(t,flip(dataA(:,2)),'filled','d','MarkerFaceColor',color{6}); hold on
    plot(t,dataA(:,1),'Color',color{5}); hold on; 
    plot(t,flip(dataA(:,2)),'Color',color{6})
    grid on; axis padded; title('Histeresis en termo-resistencia: Experimento 1','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2",'location','southeast','Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

subplot(2,2,4)
    scatter(t,dataA(:,3),'filled','d','MarkerFaceColor',color{7}); hold on
    scatter(t,flip(dataA(:,4)),'filled','d','MarkerFaceColor',color{8}); hold on
    plot(t,dataA(:,3),'Color',color{7}); hold on; 
    plot(t,flip(dataA(:,4)),'Color',color{8})
    grid on; axis padded; title('Histeresis en termo-resistencia: Experimento 2','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 3","Medici\'on 4",'location','southeast','Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

%% Comparación con la regresión

figure('Name','Incertidumbre expandida')
    %scatter(t,dataA(:,1),'filled','d','MarkerFaceColor',color{2}); hold on
    %plot(t,dataA(:,1),'Color',color{2}); hold on
    %plot(t,dataA(:,1)-UA(1),'m--',t,dataA(:,1)+UA(1),'m--','Color','#FF0000'); 

subplot(2,2,[1 2])
    for itt = 1:4; scatter(t+df1(itt),dataM(:,itt),'filled','d','MarkerFaceColor',color{itt}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataM(:,itt)-UM(itt),'m--',t+df1(itt),dataM(:,itt)+UM(itt),'m--','Color','#FF0000'); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataM(:,itt),'Color',color{itt}); hold on; end
    grid on; axis padded; title('Mediciones de temperatura con termometro de Mercurio','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4","Intervalo al 95\%",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

 subplot(2,2,[3 4])
    for itt = 1:4; scatter(t+df1(itt),dataA(:,itt),'filled','d','MarkerFaceColor',color{itt}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataA(:,itt)-UM(itt),'m--',t+df1(itt),dataA(:,itt)+UM(itt),'m--','Color','#FF0000'); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataA(:,itt),'Color',color{itt}); hold on; end
    grid on; axis padded; title('Mediciones de temperatura con termometro de Mercurio','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4","Intervalo al 95\%",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Temperatura}~[~^{\circ}C]$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;
    
%% Estados

figure('Name','Graficas de estados')

% scatter(t,dataAE(:,1))

subplot(2,2,[1 2])
    for itt = 1:4; scatter(t+df1(itt),dataME(:,itt),'filled','d','MarkerFaceColor',color{itt}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataME(:,itt),'Color',color{itt}); hold on; end
    grid on; axis padded; title('Termometro de Mercurio: Estado de la variable','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Estado}$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;

subplot(2,2,[3 4])
    for itt = 1:4; scatter(t+df1(itt),dataAE(:,itt),'filled','d','MarkerFaceColor',color{itt+4}); hold on; end
    hold on
    for itt = 1:4; plot(t+df1(itt),dataAE(:,itt),'Color',color{itt+4}); hold on; end
    grid on; axis padded; title('Termo-resistor: Estado de la variable','FontSize',14,'Interpreter','latex')
    legend("Medici\'on 1","Medici\'on 2","Medici\'on 3","Medici\'on 4",'Interpreter','latex')
    xlh = xlabel('t [s]','FontSize',12,'Interpreter','latex');
    ylabel("$$\mathrm{Estado}$$",'FontSize',12,'Interpreter','latex')
    xlh.Position(1) = xlh.Position(1) + abs(xlh.Position(1))*1.1;
    xlh.Position(2) = xlh.Position(2) + abs(xlh.Position(2))*1.5;