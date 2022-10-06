function get_weather
    set -l response (curl "https://api.openweathermap.org/data/2.5/weather?q=Denham+Springs&units=imperial&appid=$OPEN_WEATHER_API_KEY")
    echo $response > /tmp/current_weather.json
end
