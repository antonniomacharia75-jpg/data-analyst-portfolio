/*
    Covid-19 Dataset — Data Exploration
    Author : Antonnio Macharia
    Date   : 2026-03-02

    Skills demonstrated:
    Joins, CTEs, Temp Tables, Window Functions,
    Aggregate Functions, Views, Data Type Conversion
*/

-- ─────────────────────────────────────────────────────────────
-- Basic overview
-- ─────────────────────────────────────────────────────────────

SELECT *
FROM coviddeaths_csv
WHERE continent IS NOT NULL
ORDER BY 3, 4;

SELECT location, total_cases, new_cases, total_deaths, population
FROM coviddeaths_csv
ORDER BY 1, 2;

-- ─────────────────────────────────────────────────────────────
-- Total Cases vs Total Deaths
-- Shows likelihood of dying if infected in a given country
-- ─────────────────────────────────────────────────────────────

SELECT
    location,
    `date`,
    total_cases,
    total_deaths,
    (total_deaths / total_cases) * 100 AS death_percentage
FROM coviddeaths_csv
WHERE location = 'Afghanistan'
ORDER BY 1, 2;

-- ─────────────────────────────────────────────────────────────
-- Total Cases vs Population
-- Shows what percentage of the population contracted Covid
-- ─────────────────────────────────────────────────────────────

SELECT
    location,
    `date`,
    population,
    total_cases,
    total_deaths,
    (total_cases / population) * 100 AS infection_rate_pct
FROM coviddeaths_csv
WHERE location = 'Africa'
ORDER BY 1, 2;

-- ─────────────────────────────────────────────────────────────
-- Countries with Highest Infection Rate vs Population
-- ─────────────────────────────────────────────────────────────

SELECT
    location,
    population,
    MAX(total_cases)                         AS highest_infection_count,
    MAX(total_cases / population) * 100      AS percent_population_infected
FROM coviddeaths_csv
GROUP BY location, population
ORDER BY percent_population_infected DESC;

-- ─────────────────────────────────────────────────────────────
-- Countries with Highest Death Count per Population
-- ─────────────────────────────────────────────────────────────

SELECT
    location,
    SUM(total_deaths) AS total_death_count
FROM coviddeaths_csv
WHERE continent IS NOT NULL
GROUP BY location
ORDER BY total_death_count DESC;

-- ─────────────────────────────────────────────────────────────
-- BREAKDOWN BY CONTINENT
-- Continents with highest death count per population
-- ─────────────────────────────────────────────────────────────

SELECT
    continent,
    MAX(CAST(total_deaths AS UNSIGNED)) AS total_death_count
FROM coviddeaths_csv
WHERE continent IS NOT NULL
GROUP BY continent
ORDER BY total_death_count DESC;

-- ─────────────────────────────────────────────────────────────
-- GLOBAL NUMBERS — daily totals
-- ─────────────────────────────────────────────────────────────

SELECT
    `date`,
    SUM(new_cases)                                  AS total_cases,
    SUM(new_deaths)                                 AS total_deaths,
    SUM(new_deaths) / SUM(new_cases) * 100          AS death_percentage
FROM coviddeaths_csv
WHERE continent IS NOT NULL
GROUP BY `date`
ORDER BY 1;

-- ─────────────────────────────────────────────────────────────
-- Total Population vs Vaccinations
-- ─────────────────────────────────────────────────────────────

SELECT
    cd.continent,
    cd.location,
    cd.`date`,
    cd.population,
    cv.new_vaccinations
FROM coviddeaths_csv cd
JOIN covidvacinations_csv cv
    ON  cd.location = cv.location
    AND cd.`date`   = cv.`date`
WHERE cv.new_vaccinations IS NOT NULL;

-- ─────────────────────────────────────────────────────────────
-- Using CTE — Rolling vaccination count
-- ─────────────────────────────────────────────────────────────

WITH PopVsVacc (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
AS (
    SELECT
        cd.continent,
        cd.location,
        cd.`date`,
        cd.population,
        cv.new_vaccinations,
        SUM(CAST(cv.new_vaccinations AS UNSIGNED))
            OVER (PARTITION BY cd.location ORDER BY cd.location, cd.`date`)
            AS rolling_people_vaccinated
    FROM coviddeaths_csv cd
    JOIN covidvacinations_csv cv
        ON  cd.location = cv.location
        AND cd.`date`   = cv.`date`
    WHERE cv.new_vaccinations IS NOT NULL
)
SELECT
    *,
    (RollingPeopleVaccinated / Population) * 100 AS vaccination_percentage
FROM PopVsVacc;

-- ─────────────────────────────────────────────────────────────
-- Using TEMP TABLE
-- ─────────────────────────────────────────────────────────────

DROP TABLE IF EXISTS percentage_population_vaccinated;

CREATE TEMPORARY TABLE percentage_population_vaccinated (
    Continent               VARCHAR(255),
    Location                VARCHAR(255),
    Date                    DATETIME,
    Population              NUMERIC,
    New_vaccinations        NUMERIC,
    RollingPeopleVaccinated NUMERIC
);

INSERT INTO percentage_population_vaccinated
SELECT
    cd.continent,
    cd.location,
    cd.`date`,
    cd.population,
    cv.new_vaccinations,
    SUM(cv.new_vaccinations)
        OVER (PARTITION BY cv.location ORDER BY cv.location, cv.`date`)
        AS rolling_people_vaccinated
FROM coviddeaths_csv cd
JOIN covidvacinations_csv cv
    ON  cd.location = cv.location
    AND cd.`date`   = cv.`date`
WHERE cv.new_vaccinations IS NOT NULL;

SELECT
    *,
    (RollingPeopleVaccinated / Population) * 100 AS vaccination_percentage
FROM percentage_population_vaccinated;

-- ─────────────────────────────────────────────────────────────
-- Creating a View for later visualisation
-- ─────────────────────────────────────────────────────────────

CREATE OR REPLACE VIEW percentage_population_vaccinated_view AS
SELECT
    cd.continent,
    cd.location,
    cd.`date`,
    cd.population,
    cv.new_vaccinations,
    SUM(cv.new_vaccinations)
        OVER (PARTITION BY cv.location ORDER BY cv.location, cv.`date`)
        AS rolling_people_vaccinated
FROM coviddeaths_csv cd
JOIN covidvacinations_csv cv
    ON  cd.location = cv.location
    AND cd.`date`   = cv.`date`
WHERE cv.new_vaccinations IS NOT NULL;
