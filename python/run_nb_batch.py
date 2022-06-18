import papermill as pm
import multiprocessing
import os
import argparse
import json


def run_papermill(config):
    ''' Function to run notebook(s) in paralell using papermill.
    '''
    print(config)
    # get some variables from the config being run
    config = config['config'] # a bit ugly
    notebook = config['notebook']
    output_label = config["output_label"]
    
    # get name of notebook
    notebook_name = notebook.split('/')[-1].replace('.ipynb','')
    output_dir = f'papermill_outputs/{notebook_name}/{output_label}'

    # print config to be run
    print("-"*50)
    print(config)
    print("-"*50)
    
    # make output dir if need to
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    output_path = f'{output_dir}/{notebook_name}_{output_label}.ipynb'
    output_path_backup = output_path.replace('.ipynb','_backup.ipynb')

    # rename existing output file if need to
    if os.path.exists(output_path):
        os.rename(output_path,output_path_backup)
        # remove existing backup file if there is one
        #os.remove(output_path)
        # rename existing output file
        

    # run notebook using papermill
    pm.execute_notebook(
        notebook,
        output_path,
        parameters=dict(config=config)
        )


# add args
parser = argparse.ArgumentParser(description='Batch run some notebooks.')
parser.add_argument(
    '--config_file',
    type=str, 
    default='config_data_explorer.json', 
    help='point to the config file you want to use.'
    )
parser.add_argument(
    '--run_mode',
    type=str, 
    default='secuential', 
    help="If set to 'parallel', then run using multiprocessing, just sequential for any other value."
    )

# parse args
args = parser.parse_args()
config_file = args.config_file
run_mode = args.run_mode

print(config_file)
#print(run_mode)
# read in config_file
with open(config_file) as json_file:  
    configs = json.load(json_file)
    print(configs)

if __name__ == '__main__':

    # loop over each config
    for config in configs:

        # pass the config keys in a dict with known name for unpacking by the run_papermill function
        #print(config)
        config_dict = {'config':configs[config]}

        if run_mode == 'parallel':
            p = multiprocessing.Process(
                target=run_papermill,
                args=(config_dict) 
            )
            p.start()
        else:
            run_papermill(config_dict)

    
    
#python run_nb_batch.py --config_file=config_data_explorer.json
#Puedo correr multiples instancias con diferentes config files.

#python run_nb_batch.py --config_file=config_con_fe.json

#python run_nb_batch.py --config_file=config_lgbm_grid.json


#python run_nb_batch.py --config_file=config_feature_engenieering.json

#python run_nb_batch.py --config_file=config_lgbm_optuna.json

#python run_nb_batch.py --config_file=config_recortar_dataset.json