import hug
from client import Client
from data_adapter import DataAdapter
from pressure_data_adapter import PressureDataAdapter
from plot import Plot

@hug.get(output=hug.output_format.png_image)
def temperaturePlot(cities: hug.types.delimited_list(",")):
    """Rysuje prognozę temperatury dla danych miast"""
    client = Client(cities)
    data = client.fetch()
    adapter = DataAdapter(data)
    forecasts = adapter.adapt()
    citiesPlot = Plot(forecasts).plot()
    return citiesPlot

@hug.get(output=hug.output_format.png_image)
def pressurePlot(cities: hug.types.delimited_list(",")):
    """Rysuje prognozę cisnienia dla danych miast"""
    client = Client(cities)
    data = client.fetch()
    adapter = PressureDataAdapter(data)
    forecasts = adapter.adapt()
    citiesPlot = Plot(forecasts).plot()
    return citiesPlot

@hug.get()
def test(cities: hug.types.delimited_list(",")):
    """pozwala przetestowac to co robimy"""
    client = Client(cities)
    data = client.fetch()
    return data 
