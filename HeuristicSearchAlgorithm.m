%% Programado por Adolfo Arana Espíritu Santo y Francisco Vázquez. 
% Última fecha de modificación: 27/04/2021
% Aportador: Johnatan Montalvo
% Algoritmo de búsqueda para los mejores parámetros
% Idea consiste generar mallas de m^2, donde m es el número de parámetros a
% optimizar. Por ejemplo, si son dos variables, se tendrá un cuadrado de
% m^2xm^2. Este método parte de la suposición de que puntos vecinos al punto
% que se está evualuando tendrán un comportamiento similar.
% Al generar los parámetros se busca que estén igualmente espaciados.
% Este método de búsqueda es similar al algoritmo de bisección.
% Está restrtingido ahorita a dos parámetros

%Creación de parámetros iniciales
TOL = 2/3; %Tolerancia para criterio para determinar si el pulso recibido es 1 o 0
c1 = -2; %Costo del parámetro 1. Signo menos representa pérdidas. Unidades: $/(unidad de medida de parámetro)
c2 = 9; %Costo del parámetro 2. Signo más representa ganancias. Unidades: $/(unidad de medida de parámetro)
PS = ["Potencia", "Distancia Máxima", "Ganancia"]; %Para presentar las tablas
x0 = [10,30]; %Vector con valores iniciales de los parámtros
xf = [100, 250]; %Vector con valores finales de los parámtros
m = 3; %Número de puntos que quiero en cada parámetro %Restringido ahorita a m impar
maxIteraciones = 3; %Máximo número de veces que vas a hacer la malla
nt = 256; %Partición del dominio del tiempo
l = length(x0);
n = m^l; %Para la creación del grid
pI = []; %&No hay valor óptimo en la primera corrida
puntosCriterio = nt;
IT = zeros(puntosCriterio,n); %Guardaremos aquí los valores calculados
                                           %con el fin de encontrar el mejor
pulsoInicial = zeros(puntosCriterio,n);
                                  
valores = zeros(n,l); %Guardará cómo se evaluaron los puntos
                              
ji = round(m/2);
v = ji:m:n;      %Genera los puntos centrales a evaluar
X = zeros(m,l);  %Matriz que tendrá a los puntos a evaluar
M = zeros(n,l);  %Malla completa para hacer búsqueda                            
    
    for i = 1:maxIteraciones
%Creación de la malla a evaluar
        for j = 1:l
        M (:,j) = linspace(x0(j),xf(j),n);
        X(:,j) = M(v,j);
        end
        %Restricción de dos parámtros (son dos para generar la malla)
        [POT,ZF] = meshgrid(X(:,1),X(:,2)); %Termina de generar puntos
        steps = 1;
        for j = 1:m:n %Evaluación de la malla
            [pulsoInicial(:,j:j+(m-1)),IT(:,j:j+(m-1))] = evaluacion(POT(:,steps),ZF(:,steps),nt);
            valores(j:j+(m-1),:) = [POT(:,steps) ZF(:,steps)];
            steps = steps + 1;
        end
        if sum(sum(isnan(real(IT)))) == 0 %Asegura que el método no haya divergido
            [x0, xf, alerta, pI] = busqueda(IT, valores,n,M, pulsoInicial,c1,c2,pI,i,maxIteraciones,nt,TOL);  %Comienza búsqueda
            if alerta == 1 && i == 1 
               disp("No se encontró algo bueno. Favor de redefinir las condiciones de frontera en la malla");
               break;

            elseif alerta == 1 && i < maxIteraciones 
                disp("Terminó búsqueda")
                fprintf("Iteraciones: %d\n", i)
                dis("Mejores parámetros")
                table(pI(1), pI(2),c1*pI(1) + c2*pI(2), 'VariableNames', PS)
                break;

            elseif i == maxIteraciones %Poner los valores que encontró
                disp("Se alcanzó el máximo número de iteraciones")
                disp("Parámetros encontrados")
                table(x0(:,1), x0(:,2), 'VariableNames', PS(1:2))
                disp("Buenos parámetros")
                table(xf(:,1), xf(:,2), 'VariableNames', PS(1:2))
                disp("Mejor parámetro")
                table(pI(1), pI(2),c1*pI(1) + c2*pI(2), 'VariableNames', PS)
            end       
        elseif sum(sum(isnan(real(IT)))) > 0 && i == 1
            disp("Método numérico diverge")
            break;
        else 
            disp("Método numérico diverge")
            fprintf("Iteraciones: %d\n", i)
            dis("Mejores parámetros")
            table(pI(1), pI(2),c1*pI(1) + c2*pI(2), 'VariableNames', PS)
            break;
        end
        
    end %termina for iteraciones

fprintf("nt = %d, TOL = %.4f\n", nt, TOL)

function [pInc, Mat] = evaluacion(Pot,Zf,nt) %Método numérico
%Utiliza el método numérico de Fourier para resolver la NLSE.
%Programado por: Francisco Vázquez
% Última modificación: 27/04/2021
Mat = zeros(nt,length(Pot)); pInc = zeros(nt,length(Pot));

for iter = 1:length(Pot)
    %% Pramaetros de enetrada

    % Parametros del pulso y numero de puntos
        L = 8;  s = 1; zf = Zf(iter); 
        %tol = 0.15;
        %L = 10*pi; nt = 256; zf = 3; s = 1;

    % Discretizacion de los dominios
        ht = L/nt; hz = ht^2/2;
        domt = (0:(nt-1))*ht;
        ht = abs(domt(2)-domt(1)); 
        nz = ceil(zf/hz);

    % Definicion del pulso 
        pot = Pot(iter); codBin = [1, 0, 1, 1];
        amp = sqrt(pot); ah = [5.5, 5.5, 5.5, 5.5];
        ctr = [1 3 5 7];

        ft=[sech(ah(1)*(domt-ctr(1))); sech(ah(2)*(domt-ctr(2))); sech(ah(3)*(domt-ctr(3))); sech(ah(4)*(domt-ctr(4)))];
        CI=amp*codBin*ft;

    % Declaracion de variables
        sol = zeros(nt,nz);
        sol(:,1) = CI';
        sol(1,:) = sparse(1,1:nz,CI(1,1));
        sol(nt,:) = sparse(1,1:nz,CI(1,nt));
        
        
    % Metodo Numerico
        [sol] = NLSEFourier(L,sol,nt,hz,nz);
        
        Mat(:,iter) = sol(:,end);
        pInc(:,iter) = sol(:,1);

end
        

    %% Funciones
    % Euler
function [sol] = NLSEFourier(L,sol,nt,hz,nz)
%% Programado por Jonathan Montalvo
% Modificado por Francisco Vazquez
%% Grids in t and k 
    hk=2*pi/L; 
    k=(-nt/2:1:nt/2-1)*hk;
    kshift=fftshift(k); kshift2=kshift.^2;    
    
%% Initialization and computing the results
    un = sol(:,1)';

    for cuenta=1:1:nz
      F_NL = fft(exp(1i*hz*abs(un).^2).*un);
      F_D  = exp(-1i*kshift2*hz/2).*F_NL;
      un   = ifft(F_D);
      sol(:,cuenta) = un;
    end
end
end 

function [x0,x1,alerta,optimo] = busqueda(IT, valores,n,M,pulsoInicial,c1,c2,pI,iter,maxIteraciones,nt,TOL)
%% Programado por Adolfo Arana Espíritu Santo, 
% Última fecha de modificación: 27/04/2021
%  busqueda encuentra los pares (x,y) de M aplicando a la matriz IT la
%  función criterio para determinar qué valores son buenos y cuales no.
%  Pulso Inicial son parámetros usados en la función
%  criterio (verla para más información), pI es el punto para el cual se
%  generó la nueva malla (después de la primera iteración)
%  iter se compara con el número de máximo iteraciones con el 
%  fin de evaluar cómo regresar la información.
puntos = 0; %Variable que cuantifica cuántos puntos pasan el filtro
alerta = 0;
s = size(valores);
coordenadas = valores; 
DA = zeros(n,1);
%Primero busca cuáles puntos cumplen la tolerancia
%Criterio
for i = 1:n
    [filtro,DA(i)] = criterio(IT(:,i),pulsoInicial(:,i),nt,TOL);
   if filtro == 1
       puntos = puntos +1;
   else
       coordenadas(i,:) = Inf; %Es para hacer más fácil la búsqueda
   end
end
if puntos == 0
    alerta = 1;
    x0 = [];
    x1 = [];
    optimo = pI;
    
elseif iter < maxIteraciones 
    coordenadasB = coordenadas(coordenadas~=Inf); %Tamaño ((puntos*s(2)) x 1)
    coordenadas = zeros(puntos,s(2));
    for i = 1:puntos
       coordenadas(i,:) = [coordenadasB(i),coordenadasB(length(coordenadasB)/s(2)+i)];
    end
    DA = DA(DA~=Inf); %Queda vector de tamaño puntos x 1
    vec = DA == min(DA);
    minA = DA(vec);
    coordenadas = coordenadas(vec,:);
    if length(minA) > 1
        coordenadas = criterioCosto(coordenadas,c1,c2); %Me quedo con un punto
    end
            x0 = zeros(1,s(2));
            x1 = zeros(1,s(2));
            for i =1:s(2) %Es para cuando tenemos un solo punto
              ind = find(M(:,i) == coordenadas(i)); 
              if ind == 1
              x0(1,i) = M(1,i);
              x1(1,i) = M(ind + 1,i);

              elseif ind == n
              x0(1,i) = M(ind - 1,i);
              x1(1,i) = M(ind,i);
              else
              x0(1,i) = M(ind - 1,i);
              x1(1,i) = M(ind + 1,i);
              end
            end
            optimo = coordenadas;
    %Termina criterio
else 
    coordenadasB = coordenadas(coordenadas~=Inf); %Tamaño ((puntos x s(2)) x 1)
    coordenadas = zeros(puntos,s(2));
    for i = 1:puntos
       coordenadas(i,:) = [coordenadasB(i),coordenadasB(length(coordenadasB)/s(2)+i)];
    end
    x0 = coordenadas; %Todos los puntos que pasaron el filtro
    DA = DA(DA~=Inf);
    vec = DA == min(DA);
    x1 = coordenadas(vec,:); %Vectores con menor diferencia de área
    if length(x1) > 1
        optimo = criterioCosto(coordenadas(vec,:),c1,c2); %Mayor ganancia
    end
end %Termina de contar puntos 
end %Termina función

function [punto] = criterioCosto(puntos, c1,c2)
%% Programado por Adolfo Arana Espíritu Santo, 
% Última fecha de modificación: 27/04/2021
%criterioCosto aplica criterio de selección del punto basándose en el costo
%por los parámetros. Si c1 o c2 son positivos, significa ganancia, mientras
%que si son negativos, significa costo. Si se encuentran dos valores
%iguales, el siguiente criterio sería buscar disponibilidad en el mercado,
%pero por tiempo no se alcanzó a implementar, por lo que si encuentra dos o
%más con ganancias iguales, regresa el primero.
si = size(puntos);
C = zeros(si(1),1);
for j = 1:si(1)
    C(j) = c1*puntos(j,1) + c2*puntos(j,2);
end
    punto = puntos(C == max(C),:);
    punto = punto(1,:); %En caso de que salgan más de un punto 
                      %(Lo correcto sería irse a buscar qué es má factible)
end

function [filtro,dA] = criterio(resultado,inicial,nt,TOL)
%% Programado por: Francisco Vázquez
% Última modificación: 27/04/2021
%% Clasificación
% Criterio de altura. Cuenta cuántos puntos de la señal hay arriba de la
% línea generada por el valor del punto máximo del pulso original por TOL,
% con el fin de determinar si llegó un 1 o 0 y luego comparar con la señal
% original
cuad = length(abs(resultado))/4; ampCrit = max(inicial)*TOL;

    for it = 1:nt
        if resultado(it) >= ampCrit && it<=cuad; resultado(it) = resultado(it);
        elseif resultado(it) >= ampCrit && it<=2*cuad; resultado(it) = resultado(it);
        elseif resultado(it) >= ampCrit && it<=3*cuad; resultado(it) = resultado(it);
        elseif resultado(it) >= ampCrit && it<=4*cuad+1; resultado(it) = resultado(it);
        else 
            resultado(it) = 0;
        end

        if inicial(it) >= ampCrit && it<=cuad; inicial(it) =inicial(it);
        elseif inicial(it) >= ampCrit && it<=2*cuad; inicial(it) =inicial(it);
        elseif inicial(it) >= ampCrit && it<=3*cuad; inicial(it) =inicial(it);
        elseif inicial(it) >= ampCrit && it<=4*cuad+1; inicial(it) =inicial(it);
        else
            inicial(it) = 0;
        end
    end

    % Suma
        % Pulso final
            pf = cumsum(resultado');
            codF = [pf(cuad);(pf(2*cuad)-pf(cuad));(pf(3*cuad)-pf(2*cuad));(pf(4*cuad)-pf(3*cuad))];
            codF = not(ismember(codF,[0;0;0;0]));

        % Pulso inicial
            pi = cumsum(inicial');
            codI = [pi(cuad);pi(2*cuad)-pi(cuad+1);pi(3*cuad)-pi(2*cuad+1);pi(4*cuad)-pi(3*cuad+1)];
            codI = not(ismember(codI,[0;0;0;0]));
     if sum(codI == codF) == 4
    filtro = 1;
    % Diferencia
        delta = abs(pi-pf);
        dA = sum(abs([delta(cuad);delta(2*cuad)-delta(cuad+1);delta(3*cuad)-delta(2*cuad+1);delta(4*cuad)-delta(3*cuad+1);]));
    else
        filtro = 0;
        dA = Inf;
    end
end

%Búsqueda con Euler (El Algoritmo de búsqueda es igual al de Fourier, sólo
%cambia la función evaluación)

% function [pInc, Mat] = evaluacion(Pot,Zf,nt)
% %Método de Euler
% %Programado por: Francisco Vázquez
% % Última modificación: 27/04/2021
% Mat = zeros(nt,length(Pot)); pInc = zeros(nt,length(Pot));
% 
% for iter = 1:length(Pot)
%     %% Pramaetros de enetrada
% 
%     % Parametros del pulso y numero de puntos
%         L = 8;  s = 1; zf = Zf(iter); 
%         %tol = 0.15;
%         %L = 10*pi; nt = 256; zf = 3; s = 1;
% 
%     % Discretizacion de los dominios
%         ht = L/nt; hz = ht^2/2;
%         domt = (0:(nt-1))*ht;
%         ht = abs(domt(2)-domt(1)); 
%         nz = ceil(zf/hz);
% 
%     % Definicion del pulso 
%         pot = Pot(iter); codBin = [1, 0, 1, 1];
%         amp = sqrt(pot); ah = [5.5, 5.5, 5.5, 5.5];
%         ctr = [1 3 5 7]; %disp(amp./ah)
% 
%         ft=[sech(ah(1)*(domt-ctr(1))); sech(ah(2)*(domt-ctr(2))); sech(ah(3)*(domt-ctr(3))); sech(ah(4)*(domt-ctr(4)))];
%         CI=amp*codBin*ft;
% 
%     % Declaracion de variables
%         sol = zeros(nt,nz);
%         sol(:,1) = CI';
%         sol(1,:) = sparse(1,1:nz,CI(1,1));
%         sol(nt,:) = sparse(1,1:nz,CI(1,nt));
%         
%         
%     % Metodo Numerico
%         [sol] = Euler(sol,ht,hz,nt,nz,s);
%         
%         Mat(:,iter) = sol(:,end);
%         pInc(:,iter) = sol(:,1);
% 
% end
%         
% 
%     %% Funciones
%     % Euler
%     function [sol] = Euler(sol,ht,hz,nt,nz,s)
%     %% Programado por Jonathan Montalvo
%     % Modificado por Francisco Vazquez
%     % Matriz de constantes
%         eta = (hz/ht^2)*(s/(2*1i)); miu = hz/(1i);
%         A = spdiags(ones(nt,3).*[eta, -2*eta, eta],-1:1,nt,nt);
% 
%     % Iteraciones del metodo
%         for it = 1:nz
%            if it == 1
%                 sol(:,2) = A*sol(:,1) + miu*abs(sol(:,1)).^2.*sol(:,1)+sol(:,1);
%            else
%                 sol(:,it+1) = 2*(A*sol(:,it) + miu*abs(sol(:,it)).^2.*sol(:,it))+sol(:,it-1);
%            end
%         end
%     end
% end 
