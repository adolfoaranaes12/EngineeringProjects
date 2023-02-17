%Datos Simulación 
clc; close all; clear;
lambda = 660*10^-9;
puntos = 1024;
kx = 2*pi/lambda;
ky = 2*pi/lambda;
a = 1/4;

sample = exp(1i.*a.*pi);
theta = 0.15*pi/180; %ángulo entre los vectores de propagación
x = linspace(-1,1,puntos);
[X,Y] = meshgrid(x,x);
[Th,R] = cart2pol(X,Y);
a1 = ones(puntos,puntos);
m = 5; %m saltos de 2pi

M = exp(1i*pi);
E1 = a1.*exp(1i.*(kx.*cos(theta).*X + ky.*Y));
E2 = a1.*exp(1i.*(kx.*X+ky.*Y));

kk = 25;
E7 = ((a1).*exp(1i.*(kk.*R)));
E10 =a1.*exp(1i.*(X + Y));


% Datos Experimentales
close all; clc;
Delta = 1.96*0.03;
%Cargamos imágenes
%Punto 1
load("Punto1.mat")
load("Punto1B.mat")
%Punto 2
load("Punto2.mat")
load("Flama.mat")
%Punto 3
load("SinFiltro.mat")
load("Filtro.mat")
load("Filtro2.mat")
%Punto 4
%Antes polarizador
load("Punto6Max.mat")
load("Punto6Borroso.mat")
%Después polarizador
load("PolarizadorMax.mat")
load(" PolarizadorMaxGiradoAtras.mat")
load("Ortogonal.mat")
%Punto 5
load("PlacaVidrio.mat")
load("PlacaVidrioRotada.mat")
%Punto 6
load("Esferico.mat")
%Convertimos a double
Punto1 = double(rgb2gray(Punto1));
Franjas = double(rgb2gray(Franjas));

Vibraciones = double(rgb2gray(Vibraciones));
Flama = double(rgb2gray(Flama));

SinFiltro = double(rgb2gray(SinFiltro));
Filtro = double(rgb2gray(Filtro));
Filtro2 = double(rgb2gray(Filtro2));

Borroso = double(rgb2gray(Borroso));
Max = double(rgb2gray(Max));

PolarizadorMax = double(rgb2gray(PolMax));
Atras = double(rgb2gray(Atras));
Ortogonal = double(rgb2gray(Ortogonal));

Vidrio = double(rgb2gray(Vidrio));
VidrioRotado = double(rgb2gray(VidrioRotado));

Esferico = double(rgb2gray(Esferico));

% Cálculos
%Punto 2
E3 = 1/2.*E2.*M.*M;
E4 = 1/2.*E2.*M.*M;
E5 = 1/2*E1.*M;
E6 = 1/2*E1.*M.*M;
detector1 = E3+E5;
detector2 = E4+E6;
resultado = abs(detector2).^2;
kSFT = round(((max(mean(resultado)) - min(mean(resultado)))/...
   (max(mean(resultado)) + min(mean(resultado)))));

maxi = max(Punto1(Punto1 == Franjas),[],'all');
mini = min(Punto1(Punto1 == Franjas),[],'all');
params = [maxi 1; mini 1]\[1+Delta;0];
Punto1 = params(1)*Punto1 + params(2);
Franjas = params(1)*Franjas + params(2);
promP1 = mean(Punto1);
promP2 = mean(Franjas);

%Punto 5a
filtro = 0.1;
resultado3 = abs((filtro*E4)+E6).^2;
kF1T = ((max(mean(resultado3)) - min(mean(resultado3)))/...
   (max(mean(resultado3)) + min(mean(resultado3))));
%Punto 5b
resultado4 = abs(filtro*(E4  + E6)).^2;
kF2T = round(((max(mean(resultado4)) - min(mean(resultado4)))/...
   (max(mean(resultado4)) + min(mean(resultado4)))));

% Punto 6
[X, ~] = meshgrid(linspace(-5,5,1024));
k1 = 2*pi;
k2 = 0;
th = 0:360;
K = zeros(361,1);
Ea = exp(1i*k1*X);
Eb = -1i*exp(1i*k2*X);
s = size(Ea);
IG1 =zeros(s(1),s(2));
IG2 = zeros(s(1),s(2));

for ii = 1:361
EoutH = (cosd(th(ii)))^2*Ea + sind(th(ii))*cosd(th(ii))*Eb;
EoutV = sind(th(ii))*cosd(th(ii))*Ea + (sind(th(ii)))^2*Eb;

I = abs(EoutH).^2 + abs(EoutV).^2;
Imax = max(I,[],'all');
Imin = min(I,[],'all');
K(ii) = (Imax-Imin)/(Imax + Imin);
IG1 = IG1 + I*(ii==46);
IG2 = IG2 + I*(ii==136);
end
IG3 = I;
% Punto 7
E4 = 1/2.*E2.*M.*M.*sample;
resultado5 = abs(E4+E6).^2;
%Se usarán para graficar
x = 0:2.9:1944*2.9;
y = 0:2.9:1096*2.9;
save("Resultados Mach.mat")
%% Gráficas
close all
%Punto 2
figure;
imagesc(x,y,abs(detector1).^2,[0,1]);
colormap gray;
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
hold on
plot(2546.01*ones(100,1),linspace(0,y(end)))
hold off
legend("Intensidad de referencia")
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/Puerto1S.pdf"),'ContentType','vector')

figure;
imagesc(x,y,abs(detector2).^2, [0,1])
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
colormap gray
hold on
plot(2546.01*ones(100,1),linspace(0,y(end)))
hold off
legend("Intensidad de referencia")
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/Puerto2S.pdf"),'ContentType','vector')

figure;
it = mean(resultado);
fig1 = plot(linspace(0,x(end),length(it)),...
    it);
ylim([0 1])
grid on
hold on
resultado2 = mean(abs(detector1).^2);
fig2 = plot(linspace(0,x(end),length(resultado2)),...
    resultado2);

plot(2546.01*ones(100,1), linspace(0,1))
hold off
legend(["Puerto 1", "Puerto 2", "Intensidad de Referencia"],'Location','best')
xlabel("x [ua]")
ylabel("I/$I_{max}$ [ua]",'Interpreter','latex')
datatip(fig1,2546.01,it(463),'Location','northwest');
datatip(fig2,2546.01,resultado2(463),'Location','southwest');
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/ComparacionS.pdf"),'ContentType','vector')

figure;
plot(round(mean(abs(detector1).^2) + mean(abs(detector2).^2)))
xlabel("x [ua]")
ylabel("$I_1$ + $I_2$ [ua]", 'Interpreter','latex')
grid on
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/SumaS.pdf"),'ContentType','vector')

%Experimental
figure;
imagesc(x,y,Punto1), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
hold on
plot(2546.01*ones(100,1),linspace(0,y(end)))
hold off
legend("Intensidad de referencia")
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/Puerto1E.pdf"),'ContentType','vector')

figure;
imagesc(x,y,Franjas), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
hold on
plot(2546.01*ones(100,1),linspace(0,y(end)))
hold off
legend("Intensidad de referencia")
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/Puerto2E.pdf"),'ContentType','vector')

figure;
fig1 = plot(linspace(x(1),x(end),length(promP1)),promP1,'d--');
hold on
fig2 = plot(linspace(x(1),x(end),length(promP2)),promP2,'d--');
plot(2546.01*ones(100,1), linspace(0,1))
hold off
grid on
datatip(fig1,2546.01,promP1(73),'Location','southwest');
datatip(fig2,2546.01,promP2(73),'Location','southwest');
xlabel("x [\mum]")
ylabel("I/$I_{max} [ua]$",'Interpreter','latex')
legend(["Puerto 1", "Puerto 2", "Intensidad de Referencia"],'Location','best')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/ComparacionE.pdf"),'ContentType','vector')

figure;
plot(linspace(x(1),x(end),length(promP1)),promP1 + promP2, 'd--')
hold on
plot(linspace(x(1),x(end),length(promP1)),ones(length(promP1)))
plot(linspace(x(1),x(end),length(promP1)),promP1 + promP2 + 2*Delta,'y--')
plot(linspace(x(1),x(end),length(promP1)),promP1 + promP2 - 2*Delta,'y--')
hold off
grid on
legend(["Experimental", "Teórico", "Intervalo de Confianza al 95%"],'Location','best')
xlabel("x [\mum]")
ylabel("$I_1$ + $I_2 [ua]$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/SumaE.pdf"),'ContentType','vector')

figure;
plot(linspace(x(1),x(end),length(promP1)),abs((promP1 + promP2)-1)*100,'d--')
hold on
plot(linspace(x(1),x(end),length(promP1)),abs((promP1 + promP2 + 2*Delta)-1)*100,'r--')
plot(linspace(x(1),x(end),length(promP1)),abs((promP1 + promP2 - 2*Delta)-1)*100,'r--')
grid on
xlabel("x [\mum]")
ylabel("% [ua]")
legend(["Datos Eperimentales","Intervalo de Confianza al 95%"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 1/ErrorR.pdf"),'ContentType','vector')
%% Punto 3
close all
figure;
imagesc(x,y,Vibraciones), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 2/Vibraciones.pdf"),'ContentType','vector')

figure;
imagesc(x,y,Flama), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 2/Flama.pdf"),'ContentType','vector')
%% Punto 5
close all
%Simulación
%FiltroA
figure;
imagesc(resultado3/max(resultado,[],'all'), [0,1]), colormap gray
xlabel("x [ua]")
ylabel("y [ua]")
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/FiltroAS.pdf"),'ContentType','vector')

%FiltroB
figure;
imagesc(resultado4/max(resultado,[],'all')), colormap gray
xlabel("x [ua]")
ylabel("y [ua]")
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/FiltroBS.pdf"),'ContentType','vector')

figure;
s = size(resultado3);
xx = linspace(x(1),x(end),s(1));
plot(xx,mean(resultado))
hold on
plot(xx, mean(resultado3))
plot(xx, mean(resultado4))
hold off
grid on
xlabel("x [ua]")
ylabel("$I/I_{max} [ua]$", 'Interpreter','latex')
legend(["Control", "A", "B"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/ComparacionPerfilesS.pdf"),'ContentType','vector')
iT = max(mean(resultado4));

%Experimento
iMax = max(SinFiltro,[],'all');
iMin = min(SinFiltro,[],'all');
params = [iMax 1; iMin 1]\[1 + Delta;0];
SinFiltro = params(1)*SinFiltro + params(2);
Filtro = params(1)*Filtro + params(2);
Filtro2 = params(1)*Filtro2 + params(2);

promSinFiltro = mean(SinFiltro);
promFiltro = mean(Filtro);
promFiltro2 = mean(Filtro2);

figure;
imagesc(x,y,SinFiltro), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
colorbar
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/SinFiltroA.pdf"),'ContentType','vector')

xx = linspace(x(1),x(end),length(promSinFiltro));
ImaxSF = max(promSinFiltro);
IminSF = min(promSinFiltro);
kSF = (ImaxSF - IminSF)/((ImaxSF + IminSF));

figure;
imagesc(x,y,Filtro), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/FiltroAE.pdf"),'ContentType','vector')
ImaxF1 = max(promFiltro);
IminF1 = min(promFiltro);

kF1 = (ImaxF1 - IminF1)/((ImaxF1 + IminF1));

figure;
imagesc(x,y,Filtro2), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/FiltroBE.pdf"),'ContentType','vector')
ImaxF2 = max(promFiltro2);
IminF2 = min(promFiltro2);
kF2 = (ImaxF2 - IminF2)/((ImaxF2 + IminF2));

figure;
plot(xx,promSinFiltro,'bo--');
hold on
plot(xx,promFiltro,'ro--');
plot(xx,promFiltro2,'go--');
hold off
grid on
xlabel("x [\mum]")
ylabel("$I/I_{max}$", 'Interpreter','latex')
legend(["Control", "A", "B"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/ComparacionPerfilesE.pdf"),'ContentType','vector')

figure;
plot(linspace(x(1),x(end),s(1)),mean(resultado),'b')
hold on
plot(xx,promSinFiltro,'ro--');
plot(xx, promSinFiltro*(1 + Delta),'k--')
plot(xx, promSinFiltro*(1 - Delta),'k--')
hold off
grid on
xlabel("x [\mum]")
ylabel("$I/I_{max} [ua]$", 'Interpreter','latex')
legend(["Teórico", "Experimental", "Intervalo de Confianza al 95%"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/ComparacionPerfilesSF.pdf"),'ContentType','vector')

figure;
plot(linspace(x(1),x(end),s(1)),mean(resultado3),'b')
hold on
plot(xx,promFiltro,'ro--');
plot(xx, promFiltro*(1 + Delta),'k--')
plot(xx, promFiltro*(1 - Delta),'k--')
hold off
grid on
xlabel("x [\mum]")
ylabel("$I/I_{max}$", 'Interpreter','latex')
legend(["Teórico", "Experimental", "Intervalo de Confianza al 95%"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/ComparacionPerfilesFA.pdf"),'ContentType','vector')

figure;
plot(linspace(x(1),x(end),s(1)),mean(resultado4),'b')
hold on
plot(xx,promFiltro2,'ro--');
plot(xx, promFiltro2*(1 + Delta),'k--')
plot(xx, promFiltro2*(1 - Delta),'k--')
hold off
grid on
xlabel("x [\mum]")
ylabel("$I/I_{max}$", 'Interpreter','latex')
legend(["Teórico", "Experimental", "Intervalo de Confianza al 95%"])
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 3/ComparacionPerfilesFB.pdf"),'ContentType','vector')
%% Punto 6
close all
%Sin Polarizador
figure;
imagesc(x,y,Borroso/max(Borroso,[],'all')), colormap gray
c = colorbar;
xlabel("x [\mum]")
ylabel("y [\mum]")
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/SinPBorroso.pdf"),'ContentType','vector')

figure;
imagesc(x,y,Max/max(Max,[],'all')), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/SinPMax.pdf"),'ContentType','vector')

%Con Polarizador
%Simulación
figure;
imagesc(x,y,IG1/max(IG1,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/AtrasS.pdf"),'ContentType','vector')

figure;
imagesc(x,y,IG2/max(IG2,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/AdelanteS.pdf"),'ContentType','vector')

figure;
imagesc(x,y,round(IG3/max(IG3,[],'all'),2)), colormap gray
xlabel("x [ua]")
ylabel("y [ua]")
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/90S.pdf"),'ContentType','vector')

figure;
imagesc(x,y,PolarizadorMax/max(PolarizadorMax,[],'all')), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max}$ [ua]", 'Interpreter','latex')
ImaxF3 = max(PolarizadorMax,[],'all');
IminF3 = min(PolarizadorMax,[],'all');
kF3 = (ImaxF3 - IminF3)/((ImaxF3 + IminF3));
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/AdelanteE.pdf"),'ContentType','vector')

figure;
imagesc(Atras/max(Atras,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
xlabel("x [\mum]")
ylabel("y [\mum]")
ImaxF4 = max(Atras,[],'all');
IminF4 = min(Atras,[],'all');
kF4 = (ImaxF4 - IminF4)/((ImaxF4 + IminF4));
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/AtrasE.pdf"),'ContentType','vector')

figure;
imagesc(Ortogonal/max(Ortogonal,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
xlabel("x [\mum]")
ylabel("y [\mum]")
ImaxF5 = max(Ortogonal,[],'all');
IminF5 = min(Ortogonal,[],'all');
kF5 = (ImaxF5 - IminF5)/((ImaxF5 + IminF5));
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/90E.pdf"),'ContentType','vector')

figure;
plot(th,K)
hold on
errorbar(135,kF3,abs(kF3)*sqrt(2*Delta^2), 'o')
errorbar(45, kF4,abs(kF5)*sqrt(2*Delta^2),'o')
errorbar(90,kF5,abs(kF5)*sqrt(2*Delta^2),'o')
hold off
xlabel("\theta [°]")
ylabel("Visibilidad [ua]")
grid on
legend(["Teórico", "135° experimental","45° experimental","90° experimental"],...
    'Location','best')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 4/Visibilidades.pdf"),'ContentType','vector')
%% Punto 7
figure;
imagesc(x,y,resultado5/max(resultado5,[],'all')),colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 5/VidrioS.pdf"),'ContentType','vector')

figure;
imagesc(x,y,Vidrio/max(Vidrio,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 5/VidrioE.pdf"),'ContentType','vector')

figure;
imagesc(x,y,VidrioRotado/max(VidrioRotado,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max}$", 'Interpreter','latex')
xlabel("x [\mum]")
ylabel("y [\mum]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 5/VidrioER.pdf"),'ContentType','vector')
%% Punto 8
close all
figure;
imagesc(x,y,abs(E7 + E10).^2/max(abs(E7 + E10).^2,[],'all')), colormap gray
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
xlabel("x [ua]")
ylabel("y [ua]")
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 6/EsfericoS.pdf"),'ContentType','vector')

figure;
imagesc(x,y,Esferico/max(Esferico,[],'all')), colormap gray
xlabel("x [\mum]")
ylabel("y [\mum]")
c = colorbar;
ylabel(c, "$I/I_{max} [ua]$", 'Interpreter','latex')
exportgraphics(gca, strcat(pwd,"/Mach/Experimento 6/EsfericoE.pdf"),'ContentType','vector')