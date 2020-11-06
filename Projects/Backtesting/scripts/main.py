import os, sys
import platform
import requests
import zipfile
import datetime
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from scipy import stats
import seaborn as sn
import numpy as np
import matplotlib.pyplot as plt
import mplfinance as mpf
import yfinance as yf
from tqdm import tqdm

# import bokeh

def get_bossa_data():
    """Pobiera dane z DM BOSSA"""
    url = 'https://info.bossa.pl/pub/metastock/mstock/mstall.zip'
    r = requests.get(url)
    open(MAIN_PATH + SLASH + 'mstall.zip', 'wb').write(r.content)
    with zipfile.ZipFile(MAIN_PATH + SLASH + 'mstall.zip') as zip_ref:
        zip_ref.extractall(MAIN_PATH + SLASH + 'mstall')


def get_stooq_data(tickers, interval):
    """
    ticker: any ticker which can be found at stooq.pl
    interval:
    d - daily,
    w - weekly,
    m - monthly,
    q - quarterly,
    y - yearly"""
    for ticker in tqdm(tickers):
        url = 'https://stooq.pl/q/d/l/?s=' + ticker.replace('-', '_') + '&i=' + interval
        r = requests.get(url)
        path = MAIN_PATH + SLASH + 'stooq' + SLASH
        if not os.path.isdir(path):
            os.mkdir(path)
        if r.text != 'Brak danych':
            open(path + ticker + '_' + interval + '.csv', 'wb').write(r.content)
            data = pd.read_csv(path + ticker + '_' + interval + '.csv')
            data['Ticker'] = ticker
            data.to_csv(path + ticker + '_' + interval + '.csv')


def get_polish_tickers():
    tickers = pd.read_csv(MAIN_PATH + '\\tickers_pl.csv', index_col=0)
    return tickers


def get_stocks_only():
    """Zwraca słownik indeks: Lista spółek"""

    path = MAIN_PATH + SLASH + 'indeksy'
    gpw_indices = os.listdir(path)
    bossa_stocks = [i[:-4] for i in os.listdir(MAIN_PATH + SLASH + 'mstall')]
    all_stocks = {}

    for indeks in gpw_indices:
        tmp = pd.read_csv(path + SLASH + indeks, index_col=0)
        tmp.columns = ['Name', 'Rating', 'Czas', 'Kurs', 'Zmiana [pkt.]', 'Zmiana [%]',
                       'Wpływ na indeks [%]', 'Obrót', '%obrotu indeksu', '%obrotu rynku',
                       'Pakiet [szt.]', 'Wartość rynkowapakietu [pln]', 'Udział pakietu [%]']
        if len(indeks.split('.')) > 2:
            indeks_name = indeks.split('.')[0] + '.' + indeks.split('.')[1]
        else:
            indeks_name = indeks.split('.')[0]
        all_stocks[indeks_name] = list(set(tmp['Name'].to_list()) & set(bossa_stocks))

    return all_stocks


def make_index_df(all_stox):
    """
    :param all_stox: wynik funkcji get_stocks_only()
    :return:
    """
    df = pd.DataFrame(columns=['Indeks', 'Name'])
    i = 0
    for key in all_stox.keys():
        for item in all_stox[key]:
            df.loc[i] = [key.replace('-', '_').replace('.', '_'), item]
            i = i + 1
    tickers = get_polish_tickers()
    result = df.merge(tickers, how='left', on='Name')
    result.to_csv(MAIN_PATH + SLASH + 'total_indeks.csv')
    return df


def read_data(name, source='stooq', interval='d'):
    """

    :param name: nazwa papieru
    :param source: źródło danych - bos lub stooq
    :param interval: interwał danych dla danych stooq
    :return: dane historyczne papieru
    """

    tickers = get_polish_tickers()
    ticker = tickers[tickers['Name'] == name]['Ticker'].values[0]
    if source == 'bos':
        path = MAIN_PATH + SLASH + 'mstall' + SLASH
        if name[-4:] != '.mst':
            name = name.upper() + '.mst'
        if name not in os.listdir(path):
            pass
        else:
            data = pd.read_csv(path + name)
            data = data.rename(
                columns={'<TICKER>': 'Name', '<DTYYYYMMDD>': 'Date', '<OPEN>': 'Open', '<HIGH>': 'High',
                         '<LOW>': 'Low', '<CLOSE>': 'Close', '<VOL>': 'Volume'})
            data['Date'] = pd.to_datetime(data['Date'], format='%Y%m%d')
            data = data.merge(tickers, on='Name')
            data = data.fillna(method='bfill', limit=100)
            return data

    if source == 'stooq':
        path = MAIN_PATH + SLASH + 'stooq' + SLASH
        data = pd.read_csv(path + ticker + '_' + interval + '.csv', index_col=0,
                           dtype={'Ticker': object})
        data = data.rename(columns={'Data': 'Date', 'Otwarcie': 'Open', 'Najwyzszy': 'High', 'Najnizszy': 'Low',
                                    'Zamkniecie': 'Close', 'Wolumen': 'Volume'})
        data = data.merge(tickers, on='Ticker')
        data['Date'] = pd.to_datetime(data['Date'])
        data = data.fillna(method='bfill', limit=100)
        return data


def write_all_data(stocks, source='stooq', vola_window=20):
    """
    :param stocks: lista nazw papierów
    :param source: źródło danych - bos lub stooq
    :param vola_window: okres dla któego liczona jest zmienność
    :return: ramka danych z historycznymi danymi wszystkich papierów z listy stocks
    """
    result = pd.DataFrame()
    for stock in tqdm(stocks):
        new_data = read_data(name=stock, source=source)
        start = min(new_data['Date'])
        end = max(new_data['Date'])
        new_dates = pd.date_range(start, end)
        data = pd.DataFrame(columns=['Date'])
        data['Date'] = new_dates
        data = data.merge(new_data, how='left', on='Date')
        data = data.fillna(method='bfill', limit=100)
        data['Std'] = data['Close'].pct_change().rolling(vola_window).std().iloc[-1]
        data['TR'] = data['High'] - data['Low']
        data['ATR'] = data['TR'].ewm(span=vola_window).mean()
        data['MoneyVolume'] = data['Close'] * data['Volume']
        result = result.append(data)
        # result.to_csv(MAIN_PATH + SLASH + 'all_data.csv')
    print('Data loaded successfully.')
    return result


def equity_momentum_strat(all_data, stocks, date, short_term=125,
                          long_term=250, max_stocks=20, min_inv_vol=30, min_momentum=0,
                          max_gain=0.3, period_of_max_gain=90, cash_value=10000):
    """
    all_data - dane pobrane przez funkcje write_data
    stocks - tickery ktore nas interesuja
    date - data analizy w formacie DD-MM-YYYY - dni robocze!
    short_term - liczba dni w momentum 'krotkoterminowym' (krotko to pojecie wzgledne ;-))
    long_term - liczba dni w momentum długoterminowym
    max_stocks - maksymalna liczba spółek w portfelu
    min_inv_vola - tolerancja na minimalną zmienność odwróconą
    min_momentum - minimalne momentum
    max_gain - maksymalny wzrost w poniższym okresie
    period_of_max_gain - okres w którym jeśli spółka wzrośnie > max_gain to odrzucamy ją z portfela
    cash_value - gotówka przy rebalansacji
    """
    path = MAIN_PATH + SLASH + 'momentum' + SLASH
    if not os.path.isdir(path):
        os.mkdir(path)
    past = pd.to_datetime(pd.to_datetime(date) - datetime.timedelta(days=long_term))
    mask = (all_data['Date'] > past) & \
           (all_data['Date'] <= pd.to_datetime(date)) & \
           (all_data['Close'] is not np.NaN)

    all_data = all_data[mask]

    momentum = pd.DataFrame(columns=['Name', 'Ticker', 'Momentum', 'Std',
                                     'InvVola', 'Weight', 'Value', 'Price', 'Quantity'])
    today = pd.to_datetime(date)

    i = 0
    for stock in stocks:
        stock_data = all_data[all_data['Name'] == stock]
        # print(stock_data)
        ticker = all_data[all_data['Name'] == stock]['Ticker'].unique()
        # print(ticker)
        past = pd.to_datetime(today - datetime.timedelta(days=period_of_max_gain))
        cur_price = stock_data[stock_data['Date'] == today]['Close'].tail(1).mean()
        past_price = stock_data[stock_data['Date'] == past]['Close'].tail(1).mean()
        # if cur_price/past_price >= 1 + max_gain:
        #     break
        # if cur_price < stock_data[stock_data['Date'] == today]['MA100'].tail(1).mean():
        #     break
        # if all_data[all_data['Ticker'] == 'WIG']['Close'].tail(1).mean() < \
        #         all_data[all_data['Ticker'] == 'WIG']['MA200'].tail(1).mean():
        #     break
        dates = np.arange(1, (stock_data.shape[0] + 1)).reshape(-1, 1)
        log_prices = np.asarray(np.log(stock_data['Close'])).reshape(-1, 1)
        if (dates.size > 0) & (log_prices.size > 0):
            model_short = LinearRegression().fit(X=dates[:(short_term + 1)],
                                                 y=log_prices[:(short_term + 1)])
            r2_short = r2_score(log_prices[:(short_term + 1)],
                                model_short.predict(dates)[:(short_term + 1)])

            model_long = LinearRegression().fit(X=dates,
                                                y=log_prices)
            r2_long = r2_score(log_prices, model_long.predict(dates))
            slope = (model_short.coef_ + model_long.coef_) / 2
            r2 = (r2_short + r2_long) / 2
            momentum_value = np.float((np.exp(slope) ** 252 - 1)) * 100 * r2
            std = stock_data[stock_data['Date'] == pd.to_datetime(date)]['Std'].tail(1).mean()
            inv_vola = 1 / std
            price = stock_data[stock_data['Date'] == pd.to_datetime(date)]['Close'].tail(1).mean()
            momentum.loc[i] = [stock, ticker, momentum_value, std, inv_vola, 0, 0, price, 0]
            i += 1

    if not momentum.empty:
        momentum = momentum.sort_values(by=['Momentum'], ascending=False)
        momentum = momentum.head(max_stocks)
        momentum = momentum[momentum['Momentum'] > min_momentum]
        momentum = momentum[momentum['InvVola'] > min_inv_vol]
        momentum['Weight'] = momentum['InvVola'] / momentum['InvVola'].sum()
        momentum['Value'] = momentum['Weight'] * cash_value
        momentum['Quantity'] = np.round(momentum['Value'] / momentum['Price'], 0)
        momentum['Value'] = momentum['Quantity'] * momentum['Price']
        momentum['Date'] = today
        print('You can find the results at ' + path + slash + str(date)[:10] + '_' + 'momentum.csv')
        momentum.to_csv(path + str(date)[:10] + '_' + 'momentum.csv', decimal=',')
    return momentum

#TODO dokończyć update summary albo coś innego wymyślić idk

def update_summary(all_data: pd.DataFrame,
                   old_momentum: pd.DataFrame,
                   new_momentum: pd.DataFrame) -> pd.DataFrame:

    summary = pd.DataFrame(columns=['Name', 'Enter Date', 'Enter Price', 'Exit Date', 'Exit Price', 'Price Change',
                                    'Fee', 'Profit', 'Rolling Profit', 'Stocks Number', 'Comment'])
    for i in old_momentum.shape[0]:
        summary['Name'] = old_momentum.iloc[i]['Date']

    return summary


def strategy_testing(source, rebalance_dates, stocks, short_term,
                     long_term, max_stocks, vola_window, min_inv_vol, min_momentum,
                     max_gain, period_of_max_gain, cash_value):
    """
    source: stooq or bos
    rebalance_dates: lista dat w których będziemy dokonywać rebalansacji portfela
    stocks: lista spółek spośród których wybieramy
    short_term - liczba dni w momentum 'krotkoterminowym' (krotko to pojecie wzgledne ;-))
    long_term - liczba dni w momentum długoterminowym
    max_stocks - maksymalna liczba spółek w portfelu
    min_inv_vola - tolerancja na minimalną zmienność odwróconą
    min_momentum - minimalne momentum
    max_gain - maksymalny wzrost w poniższym okresie
    period_of_max_gain - okres w którym jeśli spółka wzrośnie > max_gain to odrzucamy ją z portfela
    cash_value - gotówka przy rebalansacji
    atr_range, vola_window,
    """

    all_data = write_all_data(stocks=stocks, source=source, vola_window=vola_window)

    cash_values = []
    stock_values = []
    totals = []
    dates = []
    # TODO porządna ramka podsumowująca symulację
    summary = pd.DataFrame(columns=['Name', 'Enter Date', 'Enter Price', 'Exit Date', 'Exit Price', 'Price Change',
                                    'Fee', 'Profit', 'Rolling Profit', 'Stocks Number', 'Comment'])

    cash = cash_value
    stocks_value = 0
    old_momentum = None
    for date in rebalance_dates:
        print(date)
        if date > pd.to_datetime(datetime.datetime.today()):
            break
        momentum = equity_momentum_strat(all_data=all_data,
                                         stocks=stocks,
                                         date=date,
                                         min_inv_vol=min_inv_vol,
                                         min_momentum=min_momentum,
                                         max_gain=max_gain,
                                         period_of_max_gain=period_of_max_gain,
                                         short_term=short_term,
                                         long_term=long_term,
                                         max_stocks=max_stocks,
                                         cash_value=cash)
        stocks_value_pre = 0
        provisions = []
        if old_momentum is not None:
            today = str(date)[:10]
            for stock in old_momentum['Name']:
                mask = (all_data['Date'] == today) & (all_data['Name'] == stock)
                price = max(0, all_data[mask]['Close'].tail(1).mean())
                qty = max(0, old_momentum[old_momentum['Name'] == stock]['Quantity'].tail(1).mean())
                value = price * qty
                provision = max(3, 0.0039*value)
                provisions.append(provision)
                stocks_value_pre = stocks_value_pre + value
            print('Before rebalancing')
            print('Cash:', cash)
            print('Stocks value:', stocks_value_pre)
            print('Total:', cash + stocks_value_pre)
            cash = cash + stocks_value_pre - sum(provisions)
        today = str(date)[:10]
        stocks_value_after = 0
        provisions = []
        for stock in momentum['Name']:
            mask = (all_data['Date'] == today) & (all_data['Name'] == stock)
            price = max(0, all_data[mask]['Close'].tail(1).mean())
            qty = max(0, momentum[momentum['Name'] == stock]['Quantity'].tail(1).mean())
            value = price * qty
            provision = max(3, 0.0039 * value)
            provisions.append(provision)
            stocks_value_after = stocks_value_after + price * qty
        cash = cash - stocks_value_after - sum(provisions)
        print('After rebalancing')
        print('Cash:', cash)
        print('Stocks value:', stocks_value_after)
        print('Total:', cash + stocks_value_after)
        old_momentum = momentum
        cash_values.append(cash)
        stock_values.append(stocks_value_after)
        totals.append(cash + stocks_value_after)
        dates.append(pd.to_datetime(today))

    plt.plot(dates, cash_values, 'o')
    plt.plot(dates, stock_values, 'o')
    plt.plot(dates, totals, 'o')
    plt.legend(['cash', 'stocks', 'total'])
    plt.show()


if __name__ == '__main__':

    if platform.system() == 'Windows':
        SLASH = '\\'
    else:
        SLASH = '/'

    MAIN_PATH = SLASH.join(os.getcwd().split(SLASH)[0:-1])
    gpw = get_stocks_only()
    all_idx = make_index_df(gpw)
    """Below input the indexes you are interested in."""
    etfs = ['betam40tr.pl', 'betaw20tr.pl', 'betaw20st.pl', 'betaw20lv.pl', 'etfdax.pl', 'etfw20l.pl', 'etfsp500.pl']
    indexes = list(gpw.keys())
    stocks = gpw['WIG']
    tickers = get_polish_tickers()
    tickers = tickers.loc[tickers['Name'].isin(stocks)]['Ticker'].to_list()

    """Hash the below line unless you want updated data."""
    # get_stooq_data(tickers + indexes, interval='d')
    # get_bossa_data()
    rebalance_dates = pd.date_range('2010-01-01', pd.to_datetime(datetime.datetime.today()), freq='BMS')
    # rebalance_dates = pd.date_range('01-01-2005', '01-01-2006', freq='BMS')
    # strategy_testing(source='stooq',
    #                  stocks=stocks,
    #                  rebalance_dates=rebalance_dates,
    #                  vola_window=20,
    #                  min_inv_vol=20,
    #                  min_momentum=0,
    #                  max_gain=0.15,
    #                  period_of_max_gain=90,
    #                  short_term=180,
    #                  long_term=365,
    #                  max_stocks=20,
    #                  cash_value=10000)
    # print(read_data('CDPROJEKT'))
    all_data = write_all_data(stocks=stocks, source='stooq', vola_window=20)
    #
    equity_momentum_strat(all_data=all_data,
                          stocks=stocks,
                          date='2020-10-09',
                          min_inv_vol=20,
                          min_momentum=0,
                          max_gain=0.15,
                          period_of_max_gain=45,
                          short_term=125,
                          long_term=250,
                          max_stocks=20,
                          cash_value=10000)
