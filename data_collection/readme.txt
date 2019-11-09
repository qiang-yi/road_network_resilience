searc_nearby_facilities.py: search for nearby facilities around each spatial units (e.g. census tract). Coordinates of the nearest n facilities will be saved in the input csv file.

realtime_accessibility_clv.py: stream the real time travel time from origin to destinations. It calls the 'direction' function in dur_in_traffic.py to retrieve the O-D travel time from Google Maps Direction API.

dur_in_traffic.py: stores the 'direction' function, which will be called in realtime_accessibility_clv.py.