#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

struct data
{
	int nLin;
	int nCol;
	double erro;
};
struct quad
{
	struct quad *up;
	struct quad *down;
	struct quad *left;
	struct quad *right;
	int cc;
	double a;
	double d;
};
struct quad *createmap(int n,int m)//CriarMapa (Matriz quadruplamente encadeada)
{
	struct quad *p,*ini,*ult,*ini2;
	int i,j;
	ult=NULL;

	for(i=0;i<n;i++)	//Criação da primeira Coluna
	{
		p=(struct quad*)malloc(sizeof(struct quad));
		p->cc=0;
		p->a=0.0;
		p->d=0.0;
		p->up=NULL;
		p->down=NULL;
		p->right=NULL;
		p->left=NULL;
		if(ult)
		{
			ult->down=p;
			p->up=ult;
		}
		else
		{
			ini=p;
		}
		ult=p;
	}

	ult=NULL;	

	for(i=0;i<m-1;i++)	//Criação da primeira Linha
	{
		p=(struct quad*)malloc(sizeof(struct quad));
		p->cc=0;
		p->a=0.0;
		p->d=0.0;
		p->up=NULL;
		p->down=NULL;
		p->right=NULL;
		p->left=NULL;
		if(ult)
		{
			ult->right=p;
			p->left=ult;
		}
		else
		{
			ini->right=p;
			p->left=ini;
		}
		ult=p;
	}

	ult=ini;
	ini2=ini;

	for(i=0;i<n-1;i++)	//Encadeamento do restante da matriz
	{
		for(j=0;j<m-1;j++)
		{
			p=(struct quad*)malloc(sizeof(struct quad));
			p->cc=0;
			p->a=0.0;
			p->d=0.0;
			p->up=NULL;
			p->down=NULL;
			p->right=NULL;
			p->left=NULL;
			ult->down->right=p;
			ult->right->down=p;
			p->up=ult->right;
			p->left=ult->down;
			ult=ult->right;
		}
		ini2=ini2->down;
		ult=ini2;
	}
	return ini;
}
void printMap(struct quad *p,struct data *d, char c)//Imprimir Mapa
{
	struct quad *ini,*ult;
	int i,j;
	FILE *fp;

	ini=p;
	ult=p;	

	if(c=='c')	//IMPRIME CONDIÇÃO DE CONTORNO
	{	
		fp=fopen("cc.dat","w");

		for(i=0;i<d->nCol;i++)			
		{
			for(j=0;j<d->nLin;j++)
			{
				fprintf(fp,"%f\t",ult->a);
				ult=ult->down;
			}
			fprintf(fp,"\n");
			ini=ini->right;
			ult=ini;
		}
	}
	else	//IMPRIME VALORES DA MALHA
	{	
		fp=fopen("out.dat","w");

		for(i=0;i<d->nCol;i++)			
		{
			for(j=0;j<d->nLin;j++)
			{
				fprintf(fp,"%10.5E\t",ult->a);
				ult=ult->down;
			}
			fprintf(fp,"\n");
			ini=ini->right;
			ult=ini;
		}
	}
	fclose(fp);
}
void opDif(struct quad *p,struct data *d)//Operador Diferencial
{
	struct quad *ini,*ult;
	int i,j;
	ini=p;
	ult=p;
	d->erro=0;

	for(i=0;i<d->nLin;i++)			
	{
		for(j=0;j<d->nCol;j++)
		{
			if(ult->cc==0)
			{
				//Gauss
				ult->d=0.25*(ult->right->a+ult->left->a+ult->up->a+ult->down->a);
			}
			ult=ult->right;
		}
		ini=ini->down;
		ult=ini;
	}
}
void importCC(struct quad *p,struct data *d)//Importa valores de Contorno
{
	struct quad *ini,*ult;
	int i,j;
	double aux;
	ini=p;
	ult=p;
	FILE *im = fopen("uff.dat","r");
	int c;


	for(i=0;i<d->nLin;i++)			
	{
		for(j=0;j<d->nCol;j++)
		{
			c=fgetc(im);
			if(!feof(im))
			{
				if(c=='A')//Logo Carregada
				{
					ult->cc=1;
					ult->a=100;
				}
			}
			//Aterrando laterais
			if(j==0)
			{
				ult->a=0;//terra
				ult->cc=1;
			}
			if(j==d->nCol-1)
			{
				ult->a=0;//terra
				ult->cc=1;
			}
			if(i==d->nLin-1)
			{
				ult->a=0;//terra
				ult->cc=1;
			}
			if(i==0)
			{
				ult->a=0;//terra
				ult->cc=1;
			}

			ult=ult->right;		
		}
		ini=ini->down;
		ult=ini;
	}
	fclose(im);
}
void simetria(struct quad *p,struct data *d)//Fecha Malha Vertical e Horizontalmente
{
	struct quad *ini,*ult;
	int i,j;
	double aux;
	ini=p;
	ult=p;

	for(j=0;j<d->nCol;j++)			
	{
		for(i=0;i<d->nLin;i++)
		{
			if(i==d->nLin-1){
				ult->down=ini;//fecha malha lateral
				ini->up=ult;	
			}
			ult=ult->down;	
		}
		ini=ini->right;
		ult=ini;
	}
	ini=p;
	ult=p;
	for(j=0;j<d->nLin;j++)			
	{
		for(i=0;i<d->nCol;i++)
		{
			if(i==d->nCol-1)
			{
				ult->right=ini;//fecha malha vertical
				ini->left=ult;	
			}
			ult=ult->right;	
		}
	ini=ini->down;
	ult=ini;
	}
}
void att(struct quad *p,struct data *d)//Operador Diferencial
{
	struct quad *ini,*ult;
	int i,j,tid;
	
	d->erro=0;
	ini=p;
	ult=p;

	for(i=0;i<d->nLin;i++)			
	{
		for(j=0;j<d->nCol;j++)
		{
			if(ult->cc==0)
			{
				if(ult->a!=ult->d)
				{
					if(d->erro<fabs((ult)->d-(ult)->a))
					{
						d->erro=fabs((ult)->d-(ult)->a);
					}	
					ult->a=ult->d;
				} 
			}
			ult=ult->right;
		}
		ini=ini->down;
		ult=ini;	
	}
}
int main(int argc, char *argv[])
{
	struct data *data;
	FILE *fp;
	struct quad *map;

	//Dados: Dimensão da Malha e ERRO
	data=(struct data*)malloc(sizeof(struct data));  
	data->nLin=601;  //600 = RESOLUÇÃO DA IMAGEM
	data->nCol=601;  //600 = RESOLUÇÃO DA IMAGEM
//	data->erro=0.0;

	//Cria Malha
	map=createmap((*data).nLin,(*data).nCol);
	//Importa Condição de Contorno de um arquivo em ASCII
	importCC(map,data); 
	//Imprime Condição de Contorno num arquivo cc.dat
	printMap(map,data,'c');
	//Fecha Malha
	simetria(map,data);

	//Método da Relaxação
	double tol=1e-5;
	do{
		opDif(map,data); //Operador Diferencial
		att(map,data); //Atualização da Malha
	}while(tol<data->erro);

	printMap(map,data,'m');//Imprime a malha num arquivo out.dat

	free(data);
	free(map);
}
