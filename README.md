# Список проектов

### 1. <a href="https://github.com/IanaSerezhkina/KarpovCourses" target="_blank"><b>Cсылка</b></a> на репозиторий, где лежит решение задач финального проект на курсе KarpovCourses.

### 2. Анализ бактериопланктона (<a href="https://github.com/IanaSerezhkina/educational_project/blob/master/Bacterioplankton.R" target="_blank"><b>ссылка</b></a> на скрипт)
***Для чего написан скрипт***

Скрипт написан на языке R (в тот момент я не знала Python, только закончила на Stepik курсы по R и решила потренироваться), необходим для оптимизации рутинного анализа биологических данных. 

Скрипт читает csv-файл, собирает в таблицы необходимые численные показатели (они затем оформляются через R Markdown и выгружаются в виде отчета), затем строит необходимые графики и сохраняет их в рабочую директорию. 

Сейчас я уже не работаю в том месте, где пользовалась им, но мои бывшие коллеги до сих пор его используют в своей работе.

***Описание данных, которые принимает на вход скрипт***

Ежегодно мы проводили исследование количественного и качественного бактериального состава воды отдельных участков разных морей (количество анализируемых участков варьировало от 10 до 20 в год). На каждом участке было от 10 до 70 точек, для каждой точки воду отбирали либо в трех слоях, либо в двух, в зависимости от параметров водной толщи. Получаемые результаты были в виде csv-файла, где была представлена информация о точке, слое, форме клеток, количестве клеток, биомассе клеток. 
<a href="https://github.com/IanaSerezhkina/educational_project/blob/master/%D0%9F%D1%80%D0%B8%D0%BC%D0%B5%D1%80.csv" target="_blank"><b>Здесь</b></a> можно посмотреть пример исходных данных.

В отчет для заказчика были необходимы:

- данные о средних/минимальных/максимальных значениях показателей;
- графики в формате гистограмм с распределением численности и биомассы в разных слоях;
- распределение станций по группам в зависимости от того, в каком слое больше значения (всего рассматривали 4 группы) и построение гистограмм для этих групп.

### 3. Проект по продуктовой аналитике (<a href="https://github.com/IanaSerezhkina/educational_project/blob/master/%D0%9F%D1%80%D0%BE%D0%B5%D0%BA%D1%82%20%D0%BF%D1%80%D0%BE%D0%B4%D1%83%D0%BA%D1%82%D0%BE%D0%B2%D0%B0%D1%8F%20%D0%B0%D0%BD%D0%B0%D0%BB%D0%B8%D1%82%D0%B8%D0%BA%D0%B0.xlsx" target="_blank"><b>ссылка</b></a> на решение) 

Проект для наглядности оформлен в Excel

***Описание проекта***
Дано описание интернет-магазин дизайнерских предметов интерьера. Есть значения метрик за месяц: выручка, валовая прибыль, траты на привлечение, количество пользователей и покупателей, коэффициент повторных покупок и маржа. Также дана раскадровка сайта магазина. 

***Задачи проекта***
 - рассчитать Unit-экономику;
 - рассчитать плечи метрик и обосновать какая метри является узким местом в экономике продукта;
 - изучить воронку продаж и найти на каком шаге проблема;
 - на основании раскадровки сайта интернет-магазина и его воронки продаж подготовить список продуктовых гипотез с оценкой по ICE и рассчитанным приростом прибыли по Unit-экономике
 

