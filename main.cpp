#include <atomic>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include <algorithm>
#include <functional>
#include <unordered_set>
#include <future>

using namespace std;

#define ull unsigned long long
#define ll long long


template <class T>
struct SparceMatrix
{


    inline const T operator[](const pair<ull, ull> index){
        if (index.first>=_x || index.second>=_y) throw "Out of bounds";
        int iptr_ind=distance(iptr.begin(), upper_bound(iptr.begin(), iptr.end(), pair<ull, ull>({0, index.second}),
                                                          [](const pair<ull, ull>& f1, const pair<ull, ull>& f2){return f1.second<f2.second;}
                                                          )
                                );

        ll min_index=iptr[iptr_ind].first, max_index=iptr[iptr_ind+1].first-1;

        if (iptr_ind < 0 || index.second != iptr[iptr_ind].second-1 || (iptr_ind==iptr.size()-2 && iptr[iptr_ind].first == iptr.back().first)) return _ZERO;

        if (jptr[min_index]==index.first) return aelem[min_index];
        if (jptr[max_index]==index.first) return aelem[max_index];
        ull temp_index;
        while(max_index-min_index > 1){
            temp_index=(max_index+min_index)/2;
            if (jptr[temp_index]==index.first)
                return aelem[temp_index];
            if (jptr[temp_index]>index.first)
                max_index=temp_index;
            else
                if (jptr[temp_index]<index.first)
                    min_index=temp_index;
        }
        return _ZERO;
    }

    inline void set(const pair<ull, ull> index, T value){
        if (index.first>=_x || index.second>=_y) throw "Out of bounds";
        if (abs(_ZERO-value)<0.000001) value=_ZERO;
        if (value==_ZERO){
            if ((*this)[index]==_ZERO) return;
            int iptr_ind=distance(iptr.begin(), upper_bound(iptr.begin(), iptr.end(), std::make_pair(0, index.second), [](const pair<ull, ull>& f1, const pair<ull, ull>& f2){return f1.second<f2.second;}));
            ll min_index=iptr[iptr_ind].first, max_index=iptr[iptr_ind+1].first-1;
            if (jptr[min_index]==index.first){
                aelem.erase(aelem.begin()+min_index);
                jptr.erase(jptr.begin()+min_index);
            }else if (jptr[max_index]==index.first){
                aelem.erase(aelem.begin()+max_index);
                jptr.erase(jptr.begin()+max_index);
            }else{
                ull temp_index;
                while(max_index-min_index > 1){
                    temp_index=(max_index+min_index)/2;
                    if (jptr[temp_index]==index.first){
                        aelem.erase(aelem.begin()+temp_index);
                        jptr.erase(jptr.begin()+temp_index);
                        break;
                    }
                    if (jptr[temp_index]>index.first)
                        max_index=temp_index;
                    else
                        if (jptr[temp_index]<index.first)
                            min_index=temp_index;
                }
            }
            for (uint i=iptr_ind+1; i<iptr.size(); ++i)
                --iptr[i].first;
            if (max_index==min_index){
                if (iptr_ind!=iptr.size()-2){
                    iptr[iptr_ind].second=iptr[iptr_ind+1].second;
                    iptr.erase(iptr.begin()+iptr_ind+1);
                }
            }
        }else{
            if ((*this)[index]!=_ZERO){
                //T debugValue=(*this)[index];
                //cout << debugValue << "- OldValue\n" << value << "- CurrentValue\n\n";
                //cout.flush();
                int iptr_ind=distance(iptr.begin(), upper_bound(iptr.begin(), iptr.end(), std::make_pair(0, index.second),
                                                                  [](const pair<ull, ull>& f1, const pair<ull, ull>& f2){return f1.second<f2.second;}));
                ll min_index=iptr[iptr_ind].first, max_index=iptr[iptr_ind+1].first-1;

                //jptr[ [min_index, ... , max_index] ] - столбцы с ненулевыми значениями в заданной строке
                //if (max_index<min_index) return _ZERO; не имеет смысла тк в этой строке выбранный элемент ненулевой тк строка не может быть пустой
                //if (!jptr.size()) return _ZERO;
                if (jptr[min_index]==index.first){aelem[min_index]=value; return;} //Очевидно
                if (jptr[max_index]==index.first){aelem[max_index]=value; return;} // -||-
                while(max_index-min_index > 1){ //пока не найдены 2 элемента между которыми нужно вставить элемент, условие при верном исполнении всегда true, только если ошибочно не было установленно что !=_ZERO
                    ull temp_index=(max_index+min_index)/2;
                    if (jptr[temp_index]==index.first){ //Если средний элемент и является искомым, то
                        aelem[temp_index]=value;
                        return;
                    }
                    if (jptr[temp_index]<index.first)
                        min_index=temp_index; //Если х > средего из ненулевых столбцов, то исключаем область <= temp_index
                    else
                        if (jptr[temp_index]>index.first)
                            max_index=temp_index; //Если х < средего из ненулевых столбцов, то исключаем область >= temp_index
                }
                return; //NOT NEED
            }
            //если в данной ячейке значение == _ZERO
            int iptr_ind=distance(iptr.begin(), upper_bound(iptr.begin(), iptr.end(), std::make_pair(0, index.second), [](const pair<ull, ull>& f1, const pair<ull, ull>& f2){return f1.second<f2.second;}));
            ll min_index=(iptr_ind==-1 ? 0: iptr[iptr_ind].first);
            ll  max_index=iptr[iptr_ind+1].first-1;
            //++iptr_ind;
            //jptr[ [min_index, ... , max_index) ] - столбцы с ненулевыми значениями в заданной строке
            if (index.second != iptr[iptr_ind].second-1 || (iptr_ind==iptr.size()-2 && iptr[iptr_ind].first == iptr.back().first)){ //если выбранная строка не имеет ненулевых элементов
                aelem.insert(aelem.begin()+min_index, value);
                jptr.insert(jptr.begin()+min_index, index.first);
                for (max_index=iptr_ind; max_index<iptr.size(); ++max_index)
                    ++iptr[max_index].first;
                //ull count=iptr[iptr_ind].second-(iptr_ind!=0 ? iptr[iptr_ind-1].second : 0);
                if (index.second+1==iptr[iptr.size()-2].second) iptr[iptr_ind]={min_index, index.second+1};
                else{
                    iptr.insert(iptr.begin()+iptr_ind, {min_index, index.second+1});
                }
                return;
            }
            if (jptr[min_index]>index.first){ //если в выбраной позиции _ZERO (х меньше чем jptr[min_index])
                //вставляем элемент перед min_index
                aelem.insert(aelem.begin()+min_index, value);
                jptr.insert(jptr.begin()+min_index, index.first);
            }
            else if (jptr[max_index]<index.first){  //если в выбраной позиции _ZERO (х больше чем jptr[max_index])
                //вставляем элемент после max_index
                aelem.insert(aelem.begin()+max_index+1, value);
                jptr.insert(jptr.begin()+max_index+1, index.first);
            }
            else{ //позиция находиться между jptr[min_index] и jptr[max_index], включая
                //проверять на jptr[min_index]==index.first нет смысла так как выше было сказано что на выбранной позиции стоит _ZERO
                ull temp_index;
                while(max_index-min_index > 1){ //пока не найдены 2 элемента между которыми нужно вставить элемент
                    temp_index=(max_index+min_index)/2;
                    if (jptr[temp_index]>index.first)
                        max_index=temp_index; //Если х > средего из ненулевых столбцов, то исключаем область >= temp_index
                    else
                        if (jptr[temp_index]<index.first)
                            min_index=temp_index; //Если х < средего из ненулевых столбцов, то исключаем область <= temp_index
                }
                //max_index - индекс перед которым нужно вставить значение
                aelem.insert(aelem.begin()+max_index, value);
                jptr.insert(jptr.begin()+max_index, index.first);

            }
            for (min_index=iptr_ind+1; min_index<iptr.size(); ++min_index)
                ++iptr[min_index].first;
        }
    }

    inline bool ifLineClear(const ull& i){
        uint iptr_ind=distance(iptr.begin(), upper_bound(iptr.begin(), iptr.end(), {0, i}, [](const pair<ull, ull>& f1, const pair<ull, ull>& f2){return f1.second<f2.second;}))-1;
        return (i != iptr[iptr_ind].second-1 || (iptr_ind==iptr.size()-2 && iptr[iptr_ind].first == iptr.back().first)); //если выбранная строка не имеет ненулевых элементов

    }

    /*
    inline void swapLines(ull i1, ull i2){

        if (i1==i2) return;
        ll min_ind1, max_ind1, min_ind2, max_ind2;
        min_ind1=iptr[i1]; min_ind2=iptr[i2];
        if (i1+1==_y) max_ind1=aelem.size()-1;
        else max_ind1=iptr[i1+1]-1;
        if (i2+1==_y) max_ind2=aelem.size()-1;
        else max_ind2=iptr[i2+1]-1;
        vector<pair<pair<ull,ull>, T>> Elem1;
        vector<pair<pair<ull,ull>, T>> Elem2;
        for (ll i=min_ind1; i<max_ind1; ++i){
            Elem1.push_back({{jptr[i], i1}, (*this)[{i1, jptr[i]}]});
        }
        for (ll i=min_ind2; i<max_ind2; ++i){
            Elem2.push_back({{jptr[i], i2}, (*this)[{i2, jptr[i]}]});
        }
        for (auto now: Elem1){
            (*this).set(now.first, _ZERO);
        }
        for (auto now: Elem2){
            (*this).set(now.first, _ZERO);
        }
        for (auto now: Elem1){
            (*this).set({now.first.first, i2}, now.second);
        }
        for (auto now: Elem2){
            (*this).set({now.first.first, i1}, now.second);
        }
    }
*/
    SparceMatrix(pair<ull, ull> size, T zero):  _y(size.second), _x(size.first), _ZERO(zero), iptr({{0, _y}, {0, _y+1}}) {} //default filled by zeros
    SparceMatrix(SparceMatrix<T> &other) : _x(other._x), _y(other._y), _ZERO(other._ZERO), iptr(other.iptr), jptr(other.jptr), aelem(other.aelem){}





    void print(){
        for (uint i=0; i<_y; ++i){
            for (uint j=0; j<_x; ++j){
                cout << operator[]({j,i}) << " "; cout.flush();
            }
            cout << "\n"; cout.flush();
        }
    }

    void operator=(const SparceMatrix<T>& other){
        //std::cout << "copy = \n"; cout.flush();
        this->_x=other._x;
        this->_y=other._y;
        this->_ZERO=other._ZERO;
        this->aelem=other.aelem;
        this->jptr=other.jptr;
        this->iptr=other.iptr;
    }


    void operator=(const SparceMatrix<T>&& other){
        //std::cout << "Move = \n"; cout.flush();
        this->_x=std::move(other._x);
        this->_y=std::move(other._y);
        this->_ZERO=std::move(other._ZERO);
        this->aelem=std::move(other.aelem);
        this->jptr=std::move(other.jptr);
        this->iptr=std::move(other.iptr);
    }

    SparceMatrix<T> operator-(SparceMatrix<T>& other){
        if (this->_x!=other._x || this->_y!=other._y) throw "invalid size";
        if (other._ZERO!=_ZERO) throw "different _ZERO values";
        SparceMatrix<T> ans({_x, _y}, _ZERO);
        if (other.jptr.size()==0){
            ans=(*this);
            return ans;
        }
        if (jptr.size()==0){
            ans=other;
            transform(ans.aelem.begin(), ans.aelem.end(), ans.aelem.begin(), [](const T& x){return -x;});
            return ans;
        }
        ull ind_iptr1=0;
        ull ind_iptr2=0;
        while(ind_iptr1 < iptr.size()-1 && ind_iptr2 < other.iptr.size()-1){
            if(iptr[ind_iptr1].second<other.iptr[ind_iptr2].second){
                //в other строке other.iptr[ind_iptr2].second-1 нет элементов поэтому можем скопировать в ответ эти
                auto jptr_ind2=iptr[ind_iptr1+1].first, jptr_ind1=iptr[ind_iptr1].first;
                ull jptr_sz_insert=jptr_ind2-jptr_ind1;

                auto ans_iptr_iterator=upper_bound(ans.iptr.begin(), ans.iptr.end(), pair<ull, ull>(0, iptr[ind_iptr1].second-1), [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;});
                for (auto iter=ans_iptr_iterator; iter!=ans.iptr.end(); ++iter){
                    (*iter).first+=jptr_sz_insert;
                }
                ans.iptr.insert(ans_iptr_iterator, pair<ull, ull>(ans.jptr.size(), iptr[ind_iptr1].second));

                ans.jptr.insert(ans.jptr.end(), jptr.begin()+jptr_ind1, jptr.begin()+jptr_ind2);
                ans.aelem.insert(ans.aelem.end(), aelem.begin()+jptr_ind1, aelem.begin()+jptr_ind2);

                ++ind_iptr1;
                continue;
            }else if (iptr[ind_iptr1].second>other.iptr[ind_iptr2].second){
                auto jptr_ind2=other.iptr[ind_iptr2+1].first, jptr_ind1=other.iptr[ind_iptr2].first;
                ull jptr_sz_insert=jptr_ind2-jptr_ind1;

                auto ans_iptr_iterator=upper_bound(ans.iptr.begin(), ans.iptr.end(), pair<ull, ull>(0, other.iptr[ind_iptr2].second-1), [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;});
                for (auto iter=ans_iptr_iterator; iter!=ans.iptr.end(); ++iter){
                    (*iter).first+=jptr_sz_insert;
                }
                ans.iptr.insert(ans_iptr_iterator, pair<ull, ull>(ans.jptr.size(), other.iptr[ind_iptr2].second));

                ans.jptr.insert(ans.jptr.end(), other.jptr.begin()+jptr_ind1, other.jptr.begin()+jptr_ind2);
                vector<T> values(other.aelem.begin()+jptr_ind1, other.aelem.begin()+jptr_ind2);
                transform(values.begin(), values.end(), values.begin(), [](const T& x){return -x;});
                ans.aelem.insert(ans.aelem.end(), values.begin(), values.end());

                ++ind_iptr2;
                continue;
            }
            ull j1=iptr[ind_iptr1].first; ull j2=other.iptr[ind_iptr2].first;
            bool j1b=j1<iptr[ind_iptr1+1].first, j2b=j2<other.iptr[ind_iptr2+1].first;
            while(j1<iptr[ind_iptr1+1].first || j2<other.iptr[ind_iptr2+1].first){
                if((!j2b) || (jptr[j1]<other.jptr[j2] && j1b)){
                    ans.set({jptr[j1], iptr[ind_iptr1].second-1}, aelem[j1]);
                    ++j1;
                }else if((!j1b) || (jptr[j1]>other.jptr[j2] && j2b)){
                    ans.set({other.jptr[j2], other.iptr[ind_iptr2].second-1}, -other.aelem[j2]);
                    ++j2;
                }else{
                    ans.set({jptr[j1], iptr[ind_iptr1].second-1}, aelem[j1]-other.aelem[j2]);
                    ++j1;
                    ++j2;
                }

                j1b=j1<iptr[ind_iptr1+1].first;
                j2b=j2<other.iptr[ind_iptr2+1].first;
            }
            ++ind_iptr1;
            ++ind_iptr2;
        }
        return ans;
    }

    SparceMatrix<T> operator+(SparceMatrix<T> other){
        transform(other.aelem.begin(), other.aelem.end(), other.aelem.begin(), [](const T& x){return -x;});
        return operator-(other);
    }

    SparceMatrix<T> operator*(SparceMatrix<T> other){
        if (other._ZERO!=_ZERO) throw "different _ZERO values";
        if (_x!=other._y) throw "invalid size to multiplie";
        auto ans = SparceMatrix<T>({other._x, _y}, _ZERO);
        ull ans_x=ans._x, ans_y=ans._y;
        if (jptr.size()==0 || jptr.size()==0){ return ans;}
        ull x=max(_x, other._x), y=max(_y, other._y);
        if (max(x,y) >= 2048*8){
            ull sz=max(x,y);
            auto powe=log((long double)(max(x,y)))/log(2);
            ull power=ceil(powe);
            sz=round(pow((long double)(2), power));
            SparceMatrix<T> M1((*this));
            M1._x=sz;
            if (M1.iptr[M1.iptr.size()-2].first==M1.iptr.back().first)
                M1.iptr[M1.iptr.size()-2].second=sz;
            else{
                M1.iptr.insert(M1.iptr.begin()+(M1.iptr.size()-1), pair<ull, ull>{M1.jptr.size(), sz});
            }
            M1.iptr.back().second=sz+1;
            M1._y=sz;

            other._x=sz;
            if (other.iptr[other.iptr.size()-2].first==other.iptr.back().first)
                other.iptr[other.iptr.size()-2].second=sz;
            else{
                other.iptr.insert(other.iptr.begin()+(other.iptr.size()-1), pair<ull, ull>{other.jptr.size(), sz});
            }
            other.iptr.back().second=sz+1;
            other._y=sz;
            //M1.print(); cout << "\n\n"; cout.flush();
            //other.print(); cout << "\n\n"; cout.flush();
            StrassenMultiply(M1, other, ans, ref(sz));
            ans._x=ans_x;
            if (ans.iptr.size()>2 && ans.iptr[ans.iptr.size()-3].second==ans_y){
                ans.iptr.erase(ans.iptr.begin()+(ans.iptr.size()-2));
            }else{
                ans.iptr[ans.iptr.size()-2].second=ans_y;
            }
            ans.iptr.back().second=ans_y;
            ans._y=ans_y;
            return ans;
        }
        Multiply((*this), other, ans);
        return ans;
    }

    SparceMatrix<T> operator*(T scalar){
        SparceMatrix<T> ans(*this);
        transform(ans.aelem.begin(), ans.aelem.end(), ans.aelem.begin(), [&scalar](const T& x){return x*scalar;});
        return ans;
    }

    long double norm(){
        long double t=0;
        for (ull iptr_ind=0; iptr_ind<iptr.size()-1; ++iptr_ind){
            long double tline=0;
            for (uint j=iptr[iptr_ind].first; j<iptr[iptr_ind+1].first; ++j){
                tline+=aelem[j];
            }
            t+=pow(tline, 2);
        }
        return sqrt(t);
    }

    SparceMatrix<T> line(uint i){
        SparceMatrix<T> ans({_x, 1}, _ZERO);
        auto iptr_iter=upper_bound(iptr.begin(), iptr.end(), pair<ull,ull>{0, i}, [](const pair<ull,ull>& E1, const pair<ull,ull>& E2){return E1.second<E2.second;});

        if((*iptr_iter).second-1==i){
            ull iptr_ind=distance(iptr.begin(), iptr_iter);
            ans.jptr.insert(ans.jptr.end(), jptr.begin()+(*iptr_iter).first, jptr.begin()+iptr[iptr_ind+1].first);
            ans.aelem.insert(ans.aelem.end(), aelem.begin()+iptr[iptr_ind].first, aelem.begin()+iptr[iptr_ind+1].first);
            ans.iptr.back().first=ans.jptr.size();
            //ans.iptr.back()
        }
        return ans;

    }

    inline const ull& x(){return _x;}
    inline const ull& y(){return _y;}



    const vector<ull>& _jptr(){
        return jptr;
    }

    const vector<pair<ull,ull>>& _iptr(){
        return iptr;
    }

    ~SparceMatrix(){
        aelem.clear();
        jptr.clear();
        iptr.clear();
    }

    void Transpoce() //только для векторов!!
    {
        if (_y==1){
            //iptr transform
            iptr=vector<pair<ull, ull>>();
            for (uint j=0; j<jptr.size(); ++j)
                iptr.push_back({j, jptr[j]+1});
            if(iptr.size()==0 || iptr.back().second!=_x) iptr.push_back({jptr.size()-1, _x});
            iptr.push_back({jptr.size()-1, _x+1});
            jptr=vector<ull>(jptr.size(), 0);
            swap(_x, _y);
        }else if (_x==1){
            jptr.clear();
            for (uint iptr_ind=0; iptr_ind<iptr.size()-2; ++iptr_ind){
                //if (iptr[iptr_ind+1].first==iptr[iptr_ind].first) continue;
                jptr.push_back(iptr[iptr_ind].second-1);
            }
            if (iptr[iptr.size()-2].first!=iptr.back().first)
                jptr.push_back(_y);
            iptr.clear();
            iptr={{0, 1},{jptr.size(), 2}};
            swap(_x, _y);
        }
    }



protected:

    ull _y, _x;
    //friend SparceMatrix;
    T _ZERO;
    vector<T> aelem; //ненулевые элементы построчно
    vector<ull> jptr; //номер столбца соответствующего элемента aelem
    vector<pair<ull, ull>> iptr; //номер элемента aelem в котором происходит переход на новую строку
private:
    static void StrassenMultiply(SparceMatrix<T>& M1,
                                 SparceMatrix<T>& M2,
                                 SparceMatrix<T>& ans, const ull& init_sz){
        //перед подачей в эту функцию M1 и M2 представляют собой квадратные матрицы одинакого размера степени 2
        ull sz=M1._x;
        ans=std::move(SparceMatrix<T>({sz, sz}, M1._ZERO));
        if (M1.jptr.size()==0 || M2.jptr.size()==0) return;

        auto split_to_right_and_left = [sz](ull start_row, ull end_row, SparceMatrix<T>& M, SparceMatrix<T>& LEFT_M, SparceMatrix & RIGHT_M){
            LEFT_M.iptr.clear();
            RIGHT_M.iptr.clear();
            ull start=distance(M.iptr.begin(), upper_bound(M.iptr.begin(), M.iptr.end(), pair<ull, ull>({0, start_row}), [](const pair<ull,ull>& E1,const pair<ull,ull>& E2){return E1.second<E2.second;}));
            ull end=distance(M.iptr.begin(), upper_bound(M.iptr.begin(), M.iptr.end(), pair<ull, ull>({0, end_row}), [](const pair<ull,ull>& E1,const pair<ull,ull>& E2){return E1.second<E2.second;}));
            for (ull iptr_ind=start; iptr_ind<end; ++iptr_ind){
                ull y=M.iptr[iptr_ind].second-1;
                auto jptr_iter=lower_bound(M.jptr.begin()+M.iptr[iptr_ind].first, M.jptr.begin()+M.iptr[iptr_ind+1].first, sz/2);
                //(jptr.begin()+iptr[iptr_ind].first .. jptr_iter) относиться к LEFT_M
                if (jptr_iter!=(M.jptr.begin()+M.iptr[iptr_ind].first)){
                    LEFT_M.iptr.push_back({LEFT_M.jptr.size(), y+1-start_row});
                    LEFT_M.jptr.insert(LEFT_M.jptr.end(), M.jptr.begin()+M.iptr[iptr_ind].first, jptr_iter);
                    LEFT_M.aelem.insert(LEFT_M.aelem.end(), M.aelem.begin()+M.iptr[iptr_ind].first, M.aelem.begin()+distance(M.jptr.begin(), jptr_iter));
                }

                //(jptr_iter .. jptr.begin()+iptr[iptr_ind+1].first) относиться к RIGHT_M
                if (jptr_iter!=M.jptr.begin()+M.iptr[iptr_ind+1].first){
                    RIGHT_M.iptr.push_back({RIGHT_M.jptr.size(), y+1-start_row});
                    RIGHT_M.jptr.insert(RIGHT_M.jptr.end(), jptr_iter, M.jptr.begin()+M.iptr[iptr_ind+1].first);
                    RIGHT_M.aelem.insert(RIGHT_M.aelem.end(), M.aelem.begin()+distance(M.jptr.begin(), jptr_iter), M.aelem.begin()+M.iptr[iptr_ind+1].first);
                }
            }
            if (LEFT_M.iptr.size()==0 || LEFT_M.iptr.back().second!=LEFT_M._y){
                LEFT_M.iptr.push_back({LEFT_M.jptr.size(), LEFT_M._y});
            }
            if (RIGHT_M.iptr.size()==0 || RIGHT_M.iptr.back().second!=RIGHT_M._y){
                RIGHT_M.iptr.push_back({RIGHT_M.jptr.size(), RIGHT_M._y});
            }
            LEFT_M.iptr.push_back({LEFT_M.jptr.size(), LEFT_M._y+1});
            RIGHT_M.iptr.push_back({RIGHT_M.jptr.size(), RIGHT_M._y+1});
            transform(RIGHT_M.jptr.cbegin(), RIGHT_M.jptr.cend(), RIGHT_M.jptr.begin(), [sz](const ull& x){return x-sz/2;});
        };

        SparceMatrix<T> A11({sz/2, sz/2}, M1._ZERO),
            A12({sz/2, sz/2}, M1._ZERO),
            A21({sz/2, sz/2}, M1._ZERO),
            A22({sz/2, sz/2}, M1._ZERO);

        split_to_right_and_left(0, sz/2, M1, A11, A12);
        split_to_right_and_left(sz/2, sz, M1, A21, A22);

        SparceMatrix<T> B11({sz/2, sz/2}, M1._ZERO),
            B12({sz/2, sz/2}, M1._ZERO),
            B21({sz/2, sz/2}, M1._ZERO),
            B22({sz/2, sz/2}, M1._ZERO);

        split_to_right_and_left(0, sz/2, M2, B11, B12);
        split_to_right_and_left(sz/2, sz, M2, B21, B22);


        auto T11(A11+A22), T21(B11+B22);
        auto T12(A21+A22);
        auto T23(B12-B22);
        auto T24(B21-B11);
        auto T15(A11+A12);
        auto T16(A21-A11), T26(B11+B12);
        auto T17(A12-A22), T27(B21+B22);

        SparceMatrix<T> C11({sz/2, sz/2}, M1._ZERO),
            C12({sz/2, sz/2}, M1._ZERO),
            C21({sz/2, sz/2}, M1._ZERO),
            C22({sz/2, sz/2}, M1._ZERO);

        SparceMatrix<T> P1({sz/2, sz/2}, M1._ZERO),
            P2({sz/2, sz/2}, M1._ZERO),
            P3({sz/2, sz/2}, M1._ZERO),
            P4({sz/2, sz/2}, M1._ZERO),
            P5({sz/2, sz/2}, M1._ZERO),
            P6({sz/2, sz/2}, M1._ZERO),
            P7({sz/2, sz/2}, M1._ZERO);

        if (sz/2>=1024*8){
            if (init_sz/2==sz){ //Данное условие проверяется 7 раз при каждом умножении
                future<void> res[3]; //итого будут работать за одно умножение 21 поток

                res[0]=async(launch::async, &StrassenMultiply, ref(T11), ref(T21), ref(P1), ref(init_sz));
                res[1]=async(launch::async, &StrassenMultiply, ref(T12), ref(B11), ref(P2), ref(init_sz));
                res[2]=async(launch::async, &StrassenMultiply, ref(A11), ref(T23), ref(P3), ref(init_sz));
                StrassenMultiply(A22, T24, P4, init_sz);
                res[0].get(); res[1].get(); res[2].get();

                res[0]=async(launch::async, &StrassenMultiply, ref(T15), ref(B22), ref(P5), ref(init_sz));
                res[1]=async(launch::async, &StrassenMultiply, ref(T16), ref(T26), ref(P6), ref(init_sz));
                res[2]=async(launch::async, &StrassenMultiply, ref(T17), ref(T27), ref(P7), ref(init_sz));
                res[0].get(); res[1].get(); res[2].get();
            }else{
                StrassenMultiply(T11, T21, P1, init_sz);
                StrassenMultiply(T12, B11, P2, init_sz);
                StrassenMultiply(A11, T23, P3, init_sz);
                StrassenMultiply(A22, T24, P4, init_sz);
                StrassenMultiply(T15, B22, P5, init_sz);
                StrassenMultiply(T16, T26, P6, init_sz);
                StrassenMultiply(T17, T27, P7, init_sz);
            }
        }else{
            Multiply(T11, T21, P1);
            Multiply(T12, B11, P2);
            Multiply(A11, T23, P3);
            Multiply(A22, T24, P4);
            Multiply(T15, B22, P5);
            Multiply(T16, T26, P6);
            Multiply(T17, T27, P7);
        }
        C11=P1+P4-P5+P7;
        C12=P3+P5;
        C21=P2+P4;
        C22=P1-P2+P3+P6;

        //union
        ans.iptr.clear();
        ans.jptr.clear();
        ans.aelem.clear();
        ull iptr_indC11=0, iptr_indC12=0;
        while(iptr_indC11<C11.iptr.size()-1 || iptr_indC12<C12.iptr.size()-1){
            ull C11_row_p1=C11.iptr[iptr_indC11].second, C12_row_p1=C12.iptr[iptr_indC12].second;
            if (C11_row_p1<C12_row_p1){
                //дополняем только из C11
                ans.iptr.push_back({ans.jptr.size(), C11_row_p1});

                ans.jptr.insert(ans.jptr.end(), C11.jptr.begin()+C11.iptr[iptr_indC11].first, C11.jptr.begin()+C11.iptr[iptr_indC11+1].first);
                ans.aelem.insert(ans.aelem.end(), C11.aelem.begin()+C11.iptr[iptr_indC11].first, C11.aelem.begin()+C11.iptr[iptr_indC11+1].first);
                ++iptr_indC11;
            }else if (C11_row_p1>C12_row_p1){
                //дополняем только из C12
                ans.iptr.push_back({ans.jptr.size(), C12_row_p1});
                vector<ull> value(C12.jptr.begin()+C12.iptr[iptr_indC12].first, C12.jptr.begin()+C12.iptr[iptr_indC12+1].first);
                transform(value.begin(), value.end(), value.begin(), [sz](const ull& x){return x+sz/2;});
                ans.jptr.insert(ans.jptr.end(), value.begin(), value.end());
                ans.aelem.insert(ans.aelem.end(), C12.aelem.begin()+C12.iptr[iptr_indC12].first, C12.aelem.begin()+C12.iptr[iptr_indC12+1].first);

                ++iptr_indC12;
            }else{
                //дополняем из обоих
                ans.iptr.push_back({ans.jptr.size(), C11_row_p1});

                ans.jptr.insert(ans.jptr.end(), C11.jptr.begin()+C11.iptr[iptr_indC11].first, C11.jptr.begin()+C11.iptr[iptr_indC11+1].first);
                ans.aelem.insert(ans.aelem.end(), C11.aelem.begin()+C11.iptr[iptr_indC11].first, C11.aelem.begin()+C11.iptr[iptr_indC11+1].first);

                vector<ull> value(C12.jptr.begin()+C12.iptr[iptr_indC12].first, C12.jptr.begin()+C12.iptr[iptr_indC12+1].first);
                transform(value.begin(), value.end(), value.begin(), [sz](const ull& x){return x+sz/2;});
                ans.jptr.insert(ans.jptr.end(), value.begin(), value.end());
                ans.aelem.insert(ans.aelem.end(), C12.aelem.begin()+C12.iptr[iptr_indC12].first, C12.aelem.begin()+C12.iptr[iptr_indC12+1].first);

                ++iptr_indC11;
                ++iptr_indC12;
            }
        }
        ull iptr_indC21=0, iptr_indC22=0;
        while(iptr_indC21<C21.iptr.size()-1 || iptr_indC22<C22.iptr.size()-1){
            if (iptr_indC21==C21.iptr.size()-2 && C21.iptr[iptr_indC21].first==C21.iptr.back().first){++iptr_indC21; continue;}
            if (iptr_indC22==C22.iptr.size()-2 && C22.iptr[iptr_indC22].first==C22.iptr.back().first){++iptr_indC22; continue;}

            ull C21_row_p1=C21.iptr[iptr_indC21].second, C22_row_p1=C22.iptr[iptr_indC22].second;
            if (C21_row_p1<C22_row_p1){
                //дополняем только из C21
                if (iptr_indC21==0 && (C11.iptr[C11.iptr.size()-2].first==C11.iptr.back().first && C12.iptr[C12.iptr.size()-2].first==C12.iptr.back().first)){
                    ans.iptr.back().second=C21_row_p1+sz/2;
                }else{
                    ans.iptr.push_back({ans.jptr.size(), C21_row_p1+sz/2});
                }
                ans.jptr.insert(ans.jptr.end(), C21.jptr.begin()+C21.iptr[iptr_indC21].first, C21.jptr.begin()+C21.iptr[iptr_indC21+1].first);
                ans.aelem.insert(ans.aelem.end(), C21.aelem.begin()+C21.iptr[iptr_indC21].first, C21.aelem.begin()+C21.iptr[iptr_indC21+1].first);
                ++iptr_indC21;
            }else if (C21_row_p1>C22_row_p1){
                //дополняем только из C22

                if (iptr_indC22==0 && (C11.iptr[C11.iptr.size()-2].first==C11.iptr.back().first && C12.iptr[C12.iptr.size()-2].first==C12.iptr.back().first)){
                    ans.iptr.back().second=C22_row_p1+sz/2;
                }else{
                    ans.iptr.push_back({ans.jptr.size(), C22_row_p1+sz/2});
                }

                vector<ull> value(C22.jptr.begin()+C22.iptr[iptr_indC22].first, C22.jptr.begin()+C22.iptr[iptr_indC22+1].first);
                transform(value.begin(), value.end(), value.begin(), [sz](const ull& x){return x+sz/2;});
                ans.jptr.insert(ans.jptr.end(), value.begin(), value.end());
                ans.aelem.insert(ans.aelem.end(), C22.aelem.begin()+C22.iptr[iptr_indC22].first, C22.aelem.begin()+C22.iptr[iptr_indC22+1].first);

                ++iptr_indC22;
            }else{
                //дополняем из обоих
                if ((iptr_indC21==0 || iptr_indC22==0) && (C11.iptr[C11.iptr.size()-2].first==C11.iptr.back().first && C12.iptr[C12.iptr.size()-2].first==C12.iptr.back().first)){
                    ans.iptr.back().second=C21_row_p1+sz/2;
                }else{
                    ans.iptr.push_back({ans.jptr.size(), C21_row_p1+sz/2});
                }

                ans.jptr.insert(ans.jptr.end(), C21.jptr.begin()+C21.iptr[iptr_indC21].first, C21.jptr.begin()+C21.iptr[iptr_indC21+1].first);
                ans.aelem.insert(ans.aelem.end(), C21.aelem.begin()+C21.iptr[iptr_indC21].first, C21.aelem.begin()+C21.iptr[iptr_indC21+1].first);

                vector<ull> value(C22.jptr.begin()+C22.iptr[iptr_indC22].first, C22.jptr.begin()+C22.iptr[iptr_indC22+1].first);
                transform(value.begin(), value.end(), value.begin(), [sz](const ull& x){return x+sz/2;});
                ans.jptr.insert(ans.jptr.end(), value.begin(), value.end());
                ans.aelem.insert(ans.aelem.end(), C22.aelem.begin()+C22.iptr[iptr_indC22].first, C22.aelem.begin()+C22.iptr[iptr_indC22+1].first);

                ++iptr_indC21;
                ++iptr_indC22;
            }
        }
        if (ans.iptr.size()>0 && ans.iptr.back().first==ans.jptr.size()){
            ans.iptr.back().second=ans._y;
        }
        if (ans.iptr.size()==0 || ans.iptr.back().second!=ans._y){
            ans.iptr.push_back({ans.jptr.size(), ans._y});
        }
        ans.iptr.push_back({ans.jptr.size(), ans._y+1});
    }

    static void Multiply(SparceMatrix<T>& M1, SparceMatrix<T>& M2, SparceMatrix<T>& out){
        if (M2._ZERO!=M1._ZERO) throw "different _ZERO values";
        if (M1._x!=M2._y) throw "invalid size to multiplie";
        auto ans = SparceMatrix<T>({M2._x, M1._y}, M1._ZERO);
        if (M1.jptr.size()==0 || M2.jptr.size()==0){ out=std::move(ans); return;}
        ull iptr_i=0;
        for (; iptr_i<M1.iptr.size()-2; ++iptr_i){
            ull y=M1.iptr[iptr_i].second-1;
            vector<ull> jptr_1_line(M1.jptr.begin()+M1.iptr[iptr_i].first, M1.jptr.begin()+M1.iptr[iptr_i+1].first);
            vector<T> aelem_1_line(M1.aelem.begin()+M1.iptr[iptr_i].first, M1.aelem.begin()+M1.iptr[iptr_i+1].first);
            ull jptr1_ind=0;
            ull iptr2_q=0;
            for (; iptr2_q < M2.iptr.size()-2 && jptr1_ind<jptr_1_line.size();){
                if (jptr_1_line[jptr1_ind]<M2.iptr[iptr2_q].second-1){
                    ++jptr1_ind; continue;
                }
                if (jptr_1_line[jptr1_ind]>M2.iptr[iptr2_q].second-1){
                    iptr2_q=distance(M2.iptr.begin(), upper_bound(M2.iptr.begin()+iptr2_q+1, M2.iptr.end(), pair<ull,ull>{0, jptr_1_line[jptr1_ind]}, [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;}));
                    continue;
                }
                //==
                for(ull jptr2_ind=M2.iptr[iptr2_q].first; jptr2_ind<M2.iptr[iptr2_q+1].first; ++jptr2_ind){
                    ull x=M2.jptr[jptr2_ind];
                    T e1=aelem_1_line[jptr1_ind];
                    T e2=M2.aelem[jptr2_ind];
                    ans.set({x,y}, ans[{x,y}]+e1*e2);
                }
                ++jptr1_ind;
                ++iptr2_q;
            }
            if (jptr1_ind<jptr_1_line.size() && M2.iptr[iptr2_q].first!=M2.iptr.back().first){
                while (jptr1_ind<jptr_1_line.size() && iptr2_q<M2.iptr.size()-1){
                    if (jptr_1_line[jptr1_ind]<M2.iptr[iptr2_q].second-1){
                        ++jptr1_ind; continue;
                    }
                    if (jptr_1_line[jptr1_ind]>M2.iptr[iptr2_q].second-1){
                        iptr2_q=distance(M2.iptr.begin(), upper_bound(M2.iptr.begin()+iptr2_q+1, M2.iptr.end(), pair<ull,ull>{0, jptr_1_line[jptr1_ind]}, [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;}));
                        continue;
                    }
                    //==
                    for(ull jptr2_ind=M2.iptr[iptr2_q].first; jptr2_ind<M2.iptr.back().first; ++jptr2_ind){
                        ull x=M2.jptr[jptr2_ind];
                        T e1=aelem_1_line[jptr1_ind];
                        T e2=M2.aelem[jptr2_ind];
                        ans.set({x,y}, ans[{x,y}]+e1*e2);
                    }
                    ++jptr1_ind;
                    ++iptr2_q;
                }
            }
        }
        if (M1.iptr[iptr_i].first!=M1.iptr.back().first){
            ull y=M1.iptr[iptr_i].second-1;
            vector<ull> jptr_1_line(M1.jptr.begin()+M1.iptr[iptr_i].first, M1.jptr.begin()+M1.iptr.back().first);
            vector<T> aelem_1_line(M1.aelem.begin()+M1.iptr[iptr_i].first, M1.aelem.begin()+M1.iptr.back().first);
            ull jptr1_ind=0;
            ull iptr2_q=0;
            for (; iptr2_q < M2.iptr.size()-2 && jptr1_ind<jptr_1_line.size();){
                if (jptr_1_line[jptr1_ind]<M2.iptr[iptr2_q].second-1){
                    ++jptr1_ind; continue;
                }
                if (jptr_1_line[jptr1_ind]>M2.iptr[iptr2_q].second-1){
                    iptr2_q=distance(M2.iptr.begin(), upper_bound(M2.iptr.begin()+iptr2_q+1, M2.iptr.end(), pair<ull,ull>{0, jptr_1_line[jptr1_ind]}, [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;}));
                    continue;
                }
                //==
                for(ull jptr2_ind=M2.iptr[iptr2_q].first; jptr2_ind<M2.iptr[iptr2_q+1].first; ++jptr2_ind){
                    ull x=M2.jptr[jptr2_ind];
                    T e1=aelem_1_line[jptr1_ind];
                    T e2=M2.aelem[jptr2_ind];
                    ans.set({x,y}, ans[{x,y}]+e1*e2);
                }
                ++jptr1_ind;
                ++iptr2_q;
            }
            if (jptr1_ind<jptr_1_line.size() && M2.iptr[iptr2_q].first!=M2.iptr.back().first){
                while (jptr1_ind<jptr_1_line.size() && iptr2_q<M2.iptr.size()-1){
                    if (jptr_1_line[jptr1_ind]<M2.iptr[iptr2_q].second-1){
                        ++jptr1_ind; continue;
                    }
                    if (jptr_1_line[jptr1_ind]>M2.iptr[iptr2_q].second-1){
                        iptr2_q=distance(M2.iptr.begin(), upper_bound(M2.iptr.begin()+iptr2_q+1, M2.iptr.end(), pair<ull,ull>{0, jptr_1_line[jptr1_ind]}, [](const pair<ull, ull>& E1, const pair<ull, ull>& E2){return E1.second<E2.second;}));
                        continue;
                    }
                    //==
                    for(ull jptr2_ind=M2.iptr[iptr2_q].first; jptr2_ind<M2.iptr.back().first; ++jptr2_ind){
                        ull x=M2.jptr[jptr2_ind];
                        T e1=aelem_1_line[jptr1_ind];
                        T e2=M2.aelem[jptr2_ind];
                        ans.set({x,y}, ans[{x,y}]+e1*e2);
                    }
                    ++jptr1_ind;
                    ++iptr2_q;
                }
            }
        }
        out=std::move(ans);
    }


};


int main(){
    SparceMatrix<long double> M1({4,4}, 0.0);
                      M1.set({1,0}, 1);
    M1.set({0,1}, 2); M1.set({1,1}, 3);
                                        M1.set({2,2}, 3); M1.set({3,2}, 6);
                                                          M1.set({3,3}, 5);

    SparceMatrix<long double> M2({4,4}, 0.0);
    //<Zeros>
    M2.set({0,1}, 1); M2.set({1,1}, 2); M2.set({2,1}, 3); M2.set({3,1}, 4);
                                        M2.set({2,2}, 5); M2.set({3,2}, 1);
    M2.set({0,3}, 6);

    SparceMatrix<long double> M3({3,4}, 0.0);
    M1.print(); cout << "\n\n";
    M2.print(); cout << "\n"; cout.flush();
    M3=M1*M2;
    //M1.Multiply(M1, M2, M3);
    M3.print();
    return 0;
}
