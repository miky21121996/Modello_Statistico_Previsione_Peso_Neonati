# Modello Statistico per la Previsione del Peso Neonatale

View code [here](https://rpubs.com/miky21121996/1244031)

## Contesto Aziendale
**Azienda**: Neonatal Health Solutions  
**Obiettivo**: Creare un modello statistico in grado di prevedere con precisione il peso dei neonati alla nascita basandosi su variabili cliniche raccolte da tre ospedali. Il progetto mira a migliorare la gestione delle gravidanze ad alto rischio, ottimizzare le risorse ospedaliere e garantire migliori risultati per la salute neonatale.

### Benefici per l'Azienda e il Settore Sanitario
1. **Miglioramento delle Previsioni Cliniche**: Il peso del neonato è un indicatore chiave della salute neonatale. Previsioni accurate permettono interventi tempestivi in caso di anomalie, riducendo complicazioni perinatali.
2. **Ottimizzazione delle Risorse Ospedaliere**: Sapere in anticipo quali neonati potrebbero necessitare di cure intensive aiuta a organizzare le risorse umane e tecnologiche in modo efficiente, riducendo i costi e migliorando la pianificazione delle unità di terapia intensiva neonatale (TIN).
3. **Prevenzione e Identificazione dei Fattori di Rischio**: Il modello evidenzierà i fattori che influenzano negativamente il peso del neonato, come il fumo materno o gravidanze multiple, permettendo interventi proattivi.
4. **Valutazione delle Pratiche Ospedaliere**: Attraverso un'analisi comparativa tra i tre ospedali coinvolti, si possono identificare differenze nei risultati clinici, come una maggiore incidenza di parti cesarei, per migliorare la coerenza delle cure.
5. **Supporto alla Pianificazione Strategica**: Le previsioni possono informare politiche di salute pubblica, garantendo un impatto positivo sui tassi di mortalità e morbilità neonatale.

## Dettagli del Progetto

### 1. Raccolta dei Dati e Struttura del Dataset
Abbiamo raccolto dati su **2500 neonati** provenienti da tre ospedali. Le variabili raccolte includono:

- **Età della madre**: Età della madre in anni.
- **Numero di gravidanze**: Quante gravidanze ha avuto la madre.
- **Fumo materno**: Indicatore binario (0=non fumatrice, 1=fumatrice).
- **Durata della gravidanza**: Numero di settimane di gestazione.
- **Peso del neonato**: Peso alla nascita in grammi.
- **Lunghezza e diametro del cranio**: Lunghezza del neonato e diametro craniale, misurabili anche tramite ecografie.
- **Tipo di parto**: Natur
- **Ospedale di nascita**: Ospedale 1, 2 o 3.
- **Sesso del neonato**: Maschio (M) o femmina (F).

L'obiettivo principale è identificare quali di queste variabili sono più predittive del peso alla nascita, con particolare attenzione all'impatto del **fumo materno** e delle **settimane di gestazione**, che potrebbero indicare nascite premature.

### 2. Analisi e Modellizzazione

#### Analisi Preliminare
Nella prima fase, esploreremo le variabili attraverso un'analisi descrittiva per comprendere la distribuzione e identificare eventuali outlier o anomalie. Inoltre, verranno testate le seguenti ipotesi:

- Esiste una maggiore incidenza di parti cesarei in alcuni ospedali.
- La media del peso e della lunghezza dei neonati in questo campione è significativamente uguale a quella della popolazione.
- Le misure antropometriche (lunghezza e peso) sono significativamente diverse tra i due sessi.

#### Creazione del Modello di Regressione
Sarà sviluppato un **modello di regressione lineare multipla** che includa tutte le variabili rilevanti. Questo permetterà di quantificare l'impatto di ciascuna variabile indipendente sul peso del neonato ed esaminare eventuali interazioni. Ci aspettiamo, ad esempio, che una maggiore durata della gestazione sia associata a un peso maggiore alla nascita.

#### Selezione del Modello Ottimale
Utilizzeremo tecniche di selezione del modello come la **minimizzazione del criterio di informazione di Akaike (AIC)** o di **Bayes (BIC)** per ottenere un modello parsimonioso, eliminando le variabili non significative. Verranno considerati anche modelli con interazioni tra variabili e possibili effetti non lineari.

#### Analisi della Qualità del Modello
Valuteremo la capacità predittiva del modello attraverso metriche come l'**R²** e il **Root Mean Squared Error (RMSE)**. Faremo anche un'analisi dei residui per verificare la presenza di outlier o valori influenti che potrebbero distorcere le previsioni.

### 3. Previsioni e Risultati
Una volta validato il modello, sarà utilizzato per fare previsioni pratiche. Ad esempio, potremo stimare il peso di una neonata considerando una madre alla terza gravidanza che partorirà alla 39esima settimana.

### 4. Visualizzazioni
Utilizzeremo grafici e rappresentazioni visive per comunicare i risultati del modello e mostrare le relazioni più significative tra le variabili. Ad esempio, potremmo visualizzare l'impatto del numero di settimane di gestazione e del fumo materno sul peso previsto.

