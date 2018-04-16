// copied from https://github.com/ztellman/potemkin/blob/master/src/potemkin/PersistentMapProxy.java
package clojure.core.typed.dep.potemkin;

import clojure.lang.*;
import java.util.Iterator;
import java.util.Set;

public class PersistentMapProxy extends APersistentMap implements IEditableCollection, IObj {

  public interface IMap {
    Object get(Object k, Object defaultValue);
    Set keySet();
    IMap assoc(Object k, Object v);
    IMap dissoc(Object k);
    IMap empty();
  }

  public interface IEquality {
    boolean eq(Object o);
    int hash();
  }

  public ITransientCollection asTransient() {
    return ((IEditableCollection)PersistentHashMap.create(this)).asTransient();
  }

  public static class MapEntry extends clojure.lang.MapEntry {
    private final Object _key;
    private final ILookup _lookup;

    public MapEntry(ILookup lookup, Object key) {
      super(key, null);
      _key = key;
      _lookup = lookup;
    }

    @Override
    public Object val() {
      return _lookup.valAt(_key, null);
    }
  }

  private final IMap _map;
  private final IPersistentMap _meta;

  public PersistentMapProxy(IMap map) {
    this._map = map;
    this._meta = null;
  }

  public PersistentMapProxy(IMap map, IPersistentMap meta) {
    this._map = map;
    this._meta = meta;
  }

  public IMap innerMap() {
    return _map;
  }

  public IPersistentMap meta() {
    return _meta;
  }

  public IObj withMeta(IPersistentMap meta) {
    return new PersistentMapProxy(_map, meta);
  }

  @Override
  public int hashCode() {
    return (_map instanceof IEquality) ? ((IEquality)_map).hash() : super.hashCode();
  }

  public boolean equals(Object o) {
    if (_map instanceof IEquality) {
      IEquality map = (IEquality)_map;
      return (o instanceof PersistentMapProxy) ? map.eq(((PersistentMapProxy)o).innerMap()) : map.eq(o);
    }
    return super.equals(o);
  }

  @Override
  public boolean containsKey(Object k) {
    return _map.keySet().contains(k);
  }

  @Override
  public IMapEntry entryAt(Object k) {
    return containsKey(k) ? new MapEntry(this, k) : null;
  }

  @Override
  public IPersistentMap assoc(Object k, Object v) {
    return new PersistentMapProxy(_map.assoc(k, v));
  }

  @Override
  public IPersistentMap assocEx(Object k, Object v) {
    if (containsKey(k)) {
      throw new IllegalStateException("key already contained in map");
    }
    return assoc(k, v);
  }

  @Override
  public IPersistentMap without(Object k) {
    return new PersistentMapProxy(_map.dissoc(k));
  }

  @Override
  public Object valAt(Object k) {
    return _map.get(k, null);
  }

  @Override
  public Object valAt(Object k, Object defaultValue) {
    return _map.get(k, defaultValue);
  }

  @Override
  public int count() {
    return _map.keySet().size();
  }

  @Override
  public IPersistentCollection empty() {
    IMap empty = _map.empty();
    return empty != null ?  new PersistentMapProxy(_map.empty()) : PersistentHashMap.EMPTY;
  }

  @Override
  public Iterator iterator() {
    final Iterator i = _map.keySet().iterator();
    final ILookup l = this;
    return new Iterator() {

      @Override
      public boolean hasNext() {
        return i.hasNext();
      }

      @Override
      public Object next() {
        Object k = i.next();
        return new clojure.lang.MapEntry(k, l.valAt(k, null));
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Override
  public ISeq seq() {
    return IteratorSeq.create(iterator());
  }
}

