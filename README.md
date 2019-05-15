# TradingIndicatoR
An R package that computes some of the most known financial indicators in technical trading. Developed by **Giovanni Kraushaar**, **Paolo Montemurro** and **Luca Sanfilippo** as a small project for the course "Programming in Finance" at Università della Svizzera Italiana.

## Installation
At the moment it is only possible to install from source. The most straight forward way is using devtools via github.

```r
install.packages("devtools")
install.packages(c("knitr", "quantmod"))  # required for building vignettes
devtools::install_github('giovannikraushaar/TradingIndicatoR', build_opts = '--no-resave-data')
```

## Overview

Get introduced to the package:

```r
browseVignettes('TradingIndicatoR')
```

### Indicators
Currently `TradingIndicatoR` offers the following indicators:

|Indicator|Function|
|:--------|:-------|
|Accumulation/Distribution Line|`ADL()`|
|Aroon|`Aroon()`|
|Aroon Oscillator|`AroonOscillator()`|
|Average True Range|`ATR()`|
|Bollinger Bands|`Bollinger()`|
|Balance of Power|`BOP()`|
|Exponential Moving Average|`MA()`|
|Arms Ease of Movement|`EVM()`|
|Ichimoku Kinko Hyo|`Ichimoku()`|
|Moving Average Convergence Divergence|`MACD()`|
|On Balance Volume|`OBV()`|
|Relative Strenght Index|`RSI()`|
|Simple Moving Average|`MA()`|
|Stochastic Oscillator|`StochOscillator()`|
|Stochastic RSI|`StochRSI()`|
|Weighted Moving Average|`MA()`|