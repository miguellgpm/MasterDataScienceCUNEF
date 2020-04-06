import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import warnings
warnings.filterwarnings('ignore')

import time


def productos(data):

    dinero_disponible = input('Indique el dinero máximo que desea gastar: ')
    dinero_disponible = int(dinero_disponible)

    numero_productos = input('Seleccione el número de productos que quiera: ')
    numero_productos = int(numero_productos)

    while (numero_productos > 15) | (type(numero_productos) != int):
        print('Se ha equivocado al pulsar la tecla')
        numero_productos = input('Seleccione el número de productos que quiera: ')
        numero_productos = int(numero_productos)

    lista = []
    i = 0
    p = 1
    m = 1

    print(list(data['Producto'].unique()))
    while i < numero_productos:
        producto = input('Seleccione el producto:')

        lista2 = []

        lista2.append(p)
        lista2.append(producto)
        lista2.append(m)
        lista.append(lista2)
        i += 1

        if producto in lista:
            m = m + 1
        else:
            m = 1

        if producto in lista:
            p = p
        else:
            p += 1

    df = pd.DataFrame(lista, columns = ['IP', 'Producto', 'IM'])
    tabla = df.merge(data, how = 'inner', on = 'Producto')

    for i in range(len(tabla)):

        if i == 0:
            pass
        else:
            if tabla['Producto'][i] == tabla['Producto'][i - 1]:
                tabla['IM'][i] = tabla['IM'][i-1] + 1
            else:
                tabla['IM'][i] = 1

    for j in range(len(tabla)):
        tabla['Qmerged_label'][j] =  (tabla['IP'][j]*10 + tabla['IM'][j]).astype(float)

    print(tabla)

    favoritos = input('Si desea que haya algún producto específico en su compra escriba SI, en caso contrario escriba NO')
    favoritos = favoritos.lower()
    if favoritos == 'si':
        numero_productos_favoritos = input('Indique el número de productos que quiera introducir de forma manual:')
        numero_productos_favoritos = int(numero_productos_favoritos)

        while (numero_productos_favoritos > 15) | (type(numero_productos_favoritos) != int):
            print('Se ha equivocado al pulsar la tecla')
            numero_productos_favoritos = input('Indique el número de productos que quiera introducir de forma manual:')
            numero_productos_favoritos = int(numero_productos_favoritos)

        lista3 = []
        w = 0

        while w < numero_productos_favoritos:

            producto_favorito = input('Indique el producto:')

            while producto_favorito not in list(data['Producto'].unique()):
                print('No existe dicho producto')
                producto_favorito = input('Indique el producto:')

            marca_favorita = input('Indique la marca:')

            while marca_favorita not in list(data['Marca'].unique()):
                print('No existe dicha marca')
                marca_favorita = input('Indique la marca:')

            lista4 = []

            lista4.append(producto_favorito)
            lista4.append(marca_favorita)
            lista3.append(lista4)
            w += 1

        df2 = pd.DataFrame(lista3, columns = ['Producto', 'Marca'])

        lista5 = []
        rewards = list(np.zeros(len(tabla)))

        for t in range(len(df2)):

            posicion = tabla[(tabla['Marca'] == df2['Marca'][t]) & (tabla['Producto'] == df2['Producto'][t])].index[0]

            lista5.append(posicion)
        j = 0
        for l in lista5:
            rewards[lista5[j]] = 10.0
            j += 1
    else:
        rewards = list(np.zeros(len(tabla)))

    return tabla, rewards, dinero_disponible


def MCModelv3(data, alpha, e, epsilon, budget, reward):
    # Define the States
    Ingredients = list(set(data['IP']))
    # Initialise V_0
    V0 = data['Valor_inicial']
    data['V'] = V0
    output = []
    output1 = []
    output2 = []
    actioninfull = []
    #Interate over the number of episodes specified
    for e in range(0,e):

        episode_run = []
        #Introduce epsilon-greedy selection, we randomly select the first episode as V_0(a) = 0 for all actions
        epsilon = epsilon
        if e == 0:
            for i in range(0,len(Ingredients)):
                episode_run = np.append(episode_run,np.random.random_integers(low = 1, high = sum(1 for p in data.iloc[:, 0] if p == i+1 ), size = None))
            episode_run = episode_run.astype(int)

        else:
            for i in range(0,len(Ingredients)):
                greedyselection = np.random.random_integers(low = 1, high =10)
                if greedyselection <= (epsilon)*10:
                    episode_run = np.append(episode_run,np.random.random_integers(low = 1, high = sum(1 for p in data.iloc[:, 0] if p == i+1 ), size = None))
                else:
                    data_I = data[data['IP'] == (i+1)]
                    MaxofVforI = data_I[data_I['V'] == data_I['V'].max() ]['IM']
                    #If multiple max values, take first
                    MaxofVforI = MaxofVforI.values[0]
                    episode_run = np.append(episode_run, MaxofVforI)

                episode_run = episode_run.astype(int)



        episode = pd.DataFrame({'Producto' : Ingredients, 'Marca': episode_run})
        episode['Merged_label'] =  (episode['Producto']*10 + episode['Marca']).astype(float)
        data['Qmerged_label'] = (data['Qmerged_label']).astype(float)
        data['Reward'] = reward
        episode2 =  episode.merge(data[['Qmerged_label','Precio','Reward']], left_on='Merged_label',right_on='Qmerged_label', how = 'inner')
        data = data.drop('Reward',1)

        # Calculate our terminal reward
        if(budget >= episode2['Precio'].sum()):
            Return = 1 + (episode2['Reward'].sum())/(len(Ingredients))
            #Return = episode2['Precio'] + (episode2['Reward'].sum())/(len(Ingredients))
        else:
            Return = -1 + (episode2['Reward'].sum())/(len(Ingredients))
            #Return = -episode2['Precio'] + (episode2['Reward'].sum())/(len(Ingredients))
        episode2 = episode2.drop('Reward',1)
        episode2['Return'] = Return

        # Apply update rule to actions that were involved in obtaining terminal reward
        data = data.merge(episode2[['Merged_label','Return']], left_on='Qmerged_label',right_on='Merged_label', how = 'outer')
        data['Return'] = data['Return'].fillna(0)
        for v in range(0,len(data)):
            if data.iloc[v,9] == 0:
                data.iloc[v,7] = data.iloc[v,7]
            else:
                data.iloc[v,7]  = data.iloc[v,7]  + alpha*( (data.iloc[v,9]/len(Ingredients)) - data.iloc[v,7] )
            #data.iloc[v,7]  = data.iloc[v,7]  + alpha*( (data.iloc[v,9]/len(Ingredients)) - data.iloc[v,7] )


        # Output table
        data = data.drop('Merged_label',1)
        data = data.drop('Return',1)

        # Output is the Sum of V(a) for all episodes
        output  = np.append(output, data.iloc[:,-1].sum())

        # Ouput to optimal action from the model based on highest V(a)
        action = pd.DataFrame(data.groupby('IP')['V'].max())
        action2 = action.merge(data, left_on = 'V',right_on = 'V', how = 'inner')
        optimals = action2.loc[:,['Producto', 'Marca', 'Precio']]
        barato = pd.DataFrame(data.groupby('IP')['Precio'].min())
        barato = barato.merge(data, left_on = 'Precio',right_on = 'Precio', how = 'inner')
        barato = barato.loc[:,['Producto', 'Marca', 'Precio']]
        precio_bajo = barato['Precio'].sum()
        data.loc[:,['Producto', 'Marca', 'Precio']]
        precio_total = optimals['Precio'].sum()
        action3 = action2[['IP','IM']]
        action3 = action3.groupby('IP')['IM'].apply(lambda x :x.iloc[np.random.randint(0, len(x))])

        # Output the optimal action at each episode so we can see how this changes over time
        actioninfull = np.append(actioninfull, action3)
        actioninfull = actioninfull.astype(int)

        # Rename for clarity
        SumofV = output
        OptimalActions = action3
        ActionsSelectedinTime = actioninfull

    return(SumofV, OptimalActions, data, ActionsSelectedinTime, optimals, precio_total, barato, precio_bajo)
