CREATE EXTENSION postgis;


-- Creo tabla trollers -> 18mb
COPY trollers FROM 'C:\Users\nico_\Desktop\ITBA\TFI\global fishing watch\dataset\trollers.csv' DELIMITER '\t';
alter table trollers add column geom geometry(Geometry,4326);
update trollers
set geom = ST_SetSRID(ST_MakePoint(lon::double precision, lat::double precision),4326)
where lat is not null;

--
select * from trollers;
drop table trawlers;
create table trawlers(id serial  PRIMARY KEY, 
					  mmsi varchar,  
					  time text,
					   distance_from_shore text,
					   distance_from_port text,
					   speed text,
					   course text,
					   lat text,
					   lon text,
					   is_fishing text,
					  source text
					 );
					 

alter table trawlers add column geom geometry(Geometry,4326);
update trawlers
set geom = ST_SetSRID(ST_MakePoint(lon::double precision, lat::double precision),4326)
where lat is not null;

select * from trawlers where mmsi= '1252339803566.0'

