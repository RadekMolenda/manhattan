from data_adapter import DataAdapter

class PressureDataAdapter(DataAdapter):
	def y_values(self, city_forecast):
		forecasts = city_forecast['list']
		return list(map(lambda forecast: forecast['main']['pressure'], forecasts)) 